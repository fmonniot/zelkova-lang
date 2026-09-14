//! Walk package's module dependencies
//!
//! Within a package, each `Module` will depends on other `Module`s. The aim
//! of the dependencies module is to provide a way to sort the modules in a dependent
//! way, allowing parallel action on which module. We will also find out existing
//! cyclic dependencies and return all of them as error.
//!
//! We are currently using the excellent `petgraph` crate for sorting our dependencies
//! and discovering cycle. We currently use the results of the cycle discovery to drive
//! how we execute the processing. If we want to introduce parallel processing of the
//! different module, we will have to be a bit more thorough because we currently don't
//! have any information _at processing time_ to know when to block the check of some
//! modules. We don't know how we will do most of that processing, so it's a bit
//! premature to actually implement multi threading here.
//! Let's see how it goes first on a single thread and we can revisit this decision
//! later on.
//!

use super::name::Name;
use super::parser::Module;
use super::position::NodeSpan;
use super::source::files::SourceFileId;
use super::SpanLabel;
use log::debug;
use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::graph::{DiGraph, NodeIndex};

// TODO Improve this with barrier on each module, to be able to parallelize
// the processing down thel line.
pub struct ModuleWalker<'a> {
    /// The list of modules to process. We loose the barrier orders
    /// so we can't really process them in parallel at the moment.
    modules: Vec<&'a Module>,
}

impl<'a> std::fmt::Debug for ModuleWalker<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let modules: Vec<_> = self.modules.iter().map(|m| &m.name).collect();

        f.debug_struct("ModuleWalker")
            .field("modules", &modules)
            .finish()
    }
}

/// One `import` that forms an edge of a dependency cycle.
///
/// `from` imports `to` — the module the note lists right after it, wrapping
/// around to the cycle's start for the last edge — and `span`/`file` are where
/// that specific `import` line was written, when known.
///
/// `file` comes from the `module_files` map [`ModuleWalker::new`] is handed by
/// `compile_package`, the only place that knows which file a parsed module came
/// from — the same reasoning `ERR-5` used for [`crate::compiler::Interface::file`].
/// It is `None` for every edge built in this module's own tests, which hand-build
/// modules with nothing on disk behind them.
#[derive(Debug, PartialEq, Clone)]
pub struct CycleEdge {
    pub from: Name,
    pub to: Name,
    pub span: NodeSpan,
    pub file: Option<SourceFileId>,
}

/// A dependency cycle: one loop through a strongly-connected component of the
/// import graph, alongside the `import` that created each edge between consecutive
/// modules (wrapping around to the first), and any component member that loop does
/// not pass through.
#[derive(Debug, PartialEq, Clone)]
pub struct Cycle {
    /// *One* cycle within the strongly-connected component, in cycle order —
    /// what the summary note is built from. Not necessarily every module in the
    /// component: a component with several overlapping loops has more than one
    /// cycle in it, and picking the shortest one through a single start node (see
    /// [`cycle_walk`]) can leave others out. Finding a cycle that covers every
    /// member is the Hamiltonian cycle problem, and no such cycle need exist, so
    /// this does not try; the members left out are in [`Cycle::others`] instead,
    /// and the note names them so a user does not break one loop and immediately
    /// hit the next one hiding in the same component.
    pub path: Vec<Name>,
    /// The other modules in the same component: genuinely part of the circular
    /// dependency, but not on the particular cycle `path` names. Empty whenever
    /// `path` already covers the component, which every fixture and every real
    /// import graph seen so far is.
    pub others: Vec<Name>,
    /// `edges[i]` is the `import` written in `path[i]` that names `path[i + 1]`
    /// (or `path[0]`, for the last edge). Same length as `path`, and every entry
    /// is a real `import`, because [`cycle_walk`] only ever returns a path whose
    /// consecutive pairs are edges of the graph.
    pub edges: Vec<CycleEdge>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    CycleDetected(Vec<Cycle>),
}

/// A dependency cycle belongs to the package, not to any one module, so its message
/// names the modules involved rather than being prefixed with one of them, and its
/// labels — one per `import` that forms an edge — each carry their own file rather
/// than relying on a fallback the way a single-module phase error can
/// (`CompilationError::DependenciesError` renders this without one, see there).
impl crate::compiler::PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::CycleDetected(cycles) => format!(
                "{} circular dependenc{} between modules",
                cycles.len(),
                if cycles.len() == 1 { "y" } else { "ies" }
            ),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            // Each cycle is written back to its start, so it reads as the loop it is:
            // `A -> B -> A`. A component that holds more than the one loop gets a
            // second note naming the members that loop leaves out, so breaking it
            // does not just uncover the next cycle in the same component
            // (`Cycle::others`).
            Error::CycleDetected(cycles) => cycles
                .iter()
                .flat_map(|cycle| {
                    let mut path: Vec<String> =
                        cycle.path.iter().map(|name| name.to_string()).collect();
                    if let Some(first) = path.first().cloned() {
                        path.push(first);
                    }
                    let mut notes = vec![format!("cycle: {}", path.join(" -> "))];

                    if !cycle.others.is_empty() {
                        let others: Vec<String> =
                            cycle.others.iter().map(|name| name.to_string()).collect();
                        notes.push(format!(
                            "also part of this circular dependency, on another loop through the same modules: {}",
                            others.join(", ")
                        ));
                    }

                    notes
                })
                .collect(),
        }
    }

    fn labels(&self) -> Vec<SpanLabel> {
        match self {
            Error::CycleDetected(cycles) => cycles
                .iter()
                .flat_map(|cycle| {
                    cycle.edges.iter().filter_map(|edge| {
                        // An edge with no span (a hand-built test module) or no known
                        // file (nothing in the real pipeline, since `module_files`
                        // covers every parsed module) has nowhere honest to point;
                        // it is dropped rather than guessed at, the same rule
                        // `Interface::source_span` follows.
                        let span = edge.span.span()?;
                        let file = edge.file?;
                        Some(SpanLabel {
                            span,
                            // Every edge is equally the cause — breaking any one
                            // of them breaks the loop — so a label states only its
                            // own edge and leaves the loop to the note. Saying
                            // "closing the cycle" here would put that claim on all
                            // N labels when only the last edge closes it.
                            message: format!("`{}` imports `{}` here", edge.from, edge.to),
                            primary: true,
                            file: Some(file),
                        })
                    })
                })
                .collect(),
        }
    }
}

/// Walk `members` — one strongly-connected component `tarjan_scc` found, so every
/// node in it can reach every other — into an actual directed cycle through
/// `start`: a sequence of modules where each one really does import the next, and
/// the last one imports the first.
///
/// `tarjan_scc`'s own ordering of a component does not promise this: it is
/// membership, not a walk, so consecutive entries are not necessarily connected by
/// an edge, and the note used to print a "cycle" the user could not follow. That is
/// what `ERR-6` set out to fix, and `build_cycle` needs it to be true a second time
/// over, because it looks up the `import` behind each consecutive pair and an edge
/// that does not exist has no `import` to point a label at.
///
/// So the guarantee has to hold by construction, not by luck. This is a breadth-first
/// search from `start` over in-component edges only, stopping at the first node found
/// to import `start` back; the path is then reconstructed backwards along the BFS
/// tree. Because BFS pops nodes in non-decreasing distance order, that is the
/// *shortest* cycle through `start`, and because a BFS tree path never repeats a
/// node, it is a simple one. A greedy walk that just takes the first unvisited
/// neighbour cannot promise this — in a component with two loops sharing a node
/// (`B` imports `C` and `E`; `C` imports `D`; `D` imports `B`; `E` imports `B`) it
/// wanders into `E`, dead-ends, and hands back `[D, B, E]` even though `E` does not
/// import `D`.
///
/// The cycle it finds may be a strict subset of `members` — see [`Cycle::path`] for
/// why that is not fixable in general and what is reported instead.
///
/// `start` must be one of `members`. Callers get that from the `partition` in
/// [`ModuleWalker::new`], which is also where the component is known to be
/// non-empty; taking it as a parameter is what keeps this function off an unchecked
/// `members[0]`.
///
/// The returned path is empty only if `start` lies on no cycle at all, which a
/// component `tarjan_scc` reported with more than one member cannot produce.
fn cycle_walk(
    graph: &DiGraph<&Module, ()>,
    members: &[NodeIndex],
    start: NodeIndex,
) -> Vec<NodeIndex> {
    let member_set: HashSet<NodeIndex> = members.iter().copied().collect();

    // How each node was first reached. `start` is deliberately absent: it is the
    // target, so reaching it is what ends the search rather than something to
    // record a predecessor for.
    let mut predecessor: HashMap<NodeIndex, NodeIndex> = HashMap::new();
    let mut queue: VecDeque<NodeIndex> = VecDeque::from([start]);
    // The node that closes the loop: the first one popped that imports `start`.
    let mut closing: Option<NodeIndex> = None;

    'search: while let Some(current) = queue.pop_front() {
        for next in graph.neighbors(current).filter(|n| member_set.contains(n)) {
            if next == start {
                closing = Some(current);
                break 'search;
            }

            // First time we reach `next` is along a shortest path to it, so a
            // later, longer way in is ignored.
            if predecessor.contains_key(&next) {
                continue;
            }

            predecessor.insert(next, current);
            queue.push_back(next);
        }
    }

    let mut path = Vec::new();

    if let Some(closing) = closing {
        let mut node = closing;
        path.push(node);

        // Back up the BFS tree to `start`. `predecessor` was built by this search,
        // so every node on that chain has one until `start` is reached; the `None`
        // arm cannot fire, and stopping is the right thing if it somehow did.
        while node != start {
            match predecessor.get(&node) {
                Some(&previous) => {
                    node = previous;
                    path.push(node);
                }
                None => break,
            }
        }

        path.reverse();
    }

    path
}

/// Turn one strongly-connected component into the [`Cycle`] a diagnostic renders:
/// the module path `cycle_walk` finds, the `import` behind each edge of it, and the
/// component members that path leaves out.
///
/// `start` is the node the cycle is found through; [`ModuleWalker::new`] takes it
/// from the component itself, which is how this avoids indexing one.
fn build_cycle(
    graph: &DiGraph<&Module, ()>,
    members: &[NodeIndex],
    start: NodeIndex,
    module_files: &HashMap<Name, SourceFileId>,
) -> Cycle {
    let node_path = cycle_walk(graph, members, start);
    let on_path: HashSet<NodeIndex> = node_path.iter().copied().collect();

    let path: Vec<Name> = node_path
        .iter()
        .map(|&idx| graph[idx].name.clone())
        .collect();

    let mut others: Vec<Name> = members
        .iter()
        .filter(|idx| !on_path.contains(idx))
        .map(|&idx| graph[idx].name.clone())
        .collect();
    // `tarjan_scc`'s ordering within a component is an implementation detail; a
    // note the user reads should not vary with it. Sorted on the identifier text
    // rather than through an `Ord` on `Name`, which `name.rs` deliberately does not
    // derive.
    others.sort_by(|l, r| l.as_str().cmp(r.as_str()));

    let edges = node_path
        .iter()
        .enumerate()
        .map(|(i, &idx)| {
            let from_module = graph[idx];
            let to_idx = node_path[(i + 1) % node_path.len()];
            let to_name = graph[to_idx].name.clone();

            // The specific `import …` line in `from_module` that names `to_name` —
            // there may be several imports in `from_module`, but at most one of
            // them names this particular neighbour. `cycle_walk` guarantees this is
            // a real edge, so the only reason to find nothing is a hand-built
            // module in this file's own tests, which carry no source text.
            let import = from_module.imports.iter().find(|imp| imp.name == to_name);

            CycleEdge {
                from: from_module.name.clone(),
                to: to_name,
                span: import.map(|imp| imp.span).unwrap_or_else(NodeSpan::none),
                file: import.and_then(|_| module_files.get(&from_module.name).copied()),
            }
        })
        .collect();

    Cycle {
        path,
        others,
        edges,
    }
}

/// Put the [default imports](crate::compiler::default_imports) into the import
/// graph, so a module that never wrote `import Basics` is still checked after it.
///
/// Canonicalization resolves an implicit import against an `Interface` that already
/// exists, and nothing but this graph decides which module is checked first — so
/// without these edges whether `1 + 2` resolved would come down to the order the
/// source files happened to load in.
///
/// Two entries are skipped, and both are the reason this is not simply "an edge per
/// module per default":
///
/// - a module **named** on the list gets none, because `Basics` cannot import
///   itself and `Maybe`/`Result` would import each other
///   ([`default_imports::is_default`]);
/// - an edge whose target already depends on the importer is dropped, because
///   adding it would close a loop. `Basics` imports the `Js.Basics` facade, so
///   `Js.Basics` does not implicitly import `Basics` back.
///
/// The second test is made against the graph *as built so far* rather than against
/// the written imports alone, so no addition here can introduce a cycle that was
/// not already written in the source. `tarjan_scc` below still reports one that
/// was.
///
/// Both skips agree with [`default_imports::implicit_imports`], which drops the
/// same import for its own reason — a dropped edge means the target is checked
/// *later*, so its interface is not available when the importer is canonicalized.
///
/// # Why the loops are nested target-first
///
/// Refusing an edge against the graph *so far* means the edges already added are
/// part of what the next test sees, so this pass can talk itself out of an edge it
/// would otherwise have allowed. The nesting decides which edge wins that race, and
/// only one nesting makes the winner a property of the package rather than of the
/// alphabet.
///
/// Iterating importer-first did not. A package where `Basics` imports `Aux` and
/// `Maybe` imports `Zed` gave `Aux` its `Maybe` — nothing needs it, but nothing
/// forbade it either — and that edge made `Basics` reach `Zed` through
/// `Aux -> Maybe -> Zed`, so `Zed -> Basics` was refused and `Zed` could no longer
/// spell `+`. Renaming `Aux` to sort *after* `Zed` compiled the same package.
/// `tests/fixtures/package_default_import_priority/` is that package.
///
/// Iterating target-first, in [`default_imports::DEFAULT_IMPORTS`] order, gives the
/// race a fixed outcome twice over:
///
/// - **Across targets**, every `Basics` edge is settled before any `Maybe` edge
///   exists to constrain the graph, so a conflict is resolved in favour of whichever
///   entry the list writes first. That is the list's own stated priority, and it is
///   the same list on every package.
/// - **Within one target**, the importers cannot interfere with each other at all.
///   Adding `m -> d` creates new paths only from nodes that reach `m` to nodes `d`
///   reaches, so a *new* path `d ⇝ m'` would have to run `d ⇝ m -> d ⇝ m'` and
///   therefore require `d ⇝ m` to have held already — in which case `m -> d` was
///   refused rather than added. So every importer of `d` is tested against the graph
///   as it stood when `d`'s pass began, and the set of edges added for `d` does not
///   depend on the order they were tested in.
///
/// The same argument run with `m' = m` is why no edge added here can close a loop.
///
/// Importers are still visited in name order, but now only so that the edges land
/// in the graph in a fixed sequence: `names` is a `HashMap`, and `tarjan_scc` breaks
/// ties between unordered modules by insertion order, so without the sort the
/// *reported* check order would drift between runs even though the edge set did not.
fn add_default_import_edges(graph: &mut DiGraph<&Module, ()>, names: &HashMap<&Name, NodeIndex>) {
    let mut importers: Vec<(&Name, NodeIndex)> =
        names.iter().map(|(&name, &idx)| (name, idx)).collect();
    importers.sort_by(|left, right| left.0.as_str().cmp(right.0.as_str()));

    for default in crate::compiler::default_imports::DEFAULT_IMPORTS {
        let target = default.name();
        let Some(&target_idx) = names.get(&target) else {
            continue;
        };

        for &(module_name, module_idx) in &importers {
            if crate::compiler::default_imports::is_default(module_name) {
                continue;
            }

            // A written import already carries the edge, and replaces the implicit
            // import anyway.
            if graph[module_idx].imports.iter().any(|i| i.name == target) {
                continue;
            }

            if petgraph::algo::has_path_connecting(&*graph, target_idx, module_idx, None) {
                continue;
            }

            graph.add_edge(module_idx, target_idx, ());
        }
    }
}

impl<'a> ModuleWalker<'a> {
    /// `module_files` is how a cycle's edges learn which file each `import` was
    /// written in (`CycleEdge::file`) — this is driver code, the same as
    /// `check_in_order` below, so it is handed the map `compile_package` already
    /// built while parsing rather than trying to discover it itself. Every test in
    /// this module passes an empty map: their modules are hand-built, with no file
    /// on disk to name, and `CycleEdge::file` degrades to `None` for them exactly
    /// as documented there.
    pub fn new(
        modules: &'a [Module],
        module_files: &HashMap<Name, SourceFileId>,
    ) -> Result<ModuleWalker<'a>, Error> {
        let mut graph = DiGraph::new();

        let mut names = HashMap::new();

        // First populate the graph with all nodes, and keep a reference from Path to the node index
        for module in modules.iter() {
            let idx = graph.add_node(module);
            names.insert(&module.name, idx);
        }

        // Now that all modules have been inserted, we can create edges
        for module in modules.iter() {
            let m_idx = names.get(&module.name).unwrap();

            // When building the dependency graph, we exclude all imports for
            // modules not in this package. The reason is simple: a dependent
            // package cannot depends on the current package, so no cycle to detect
            for dep in module
                .imports
                .iter()
                .filter_map(|import| names.get(&import.name))
            {
                graph.add_edge(*m_idx, *dep, ());
            }
        }

        add_default_import_edges(&mut graph, &names);

        // Find the strongly connected graphs (scc), if there are more than one node per scc
        // it means there is a circular dependency.
        let scc = petgraph::algo::tarjan_scc(&graph);

        let (cycles, deps): (Vec<_>, Vec<_>) = scc.iter().partition(|&v| v.len() > 1);

        if cycles.is_empty() {
            let modules = deps.into_iter().flatten().map(|&idx| graph[idx]).collect();

            Ok(ModuleWalker { modules })
        } else {
            // `partition` above already established every component here has more
            // than one member, so `first()` is always `Some`; taking the start node
            // from it here, rather than indexing inside `cycle_walk`, is what keeps
            // that guarantee next to the check that establishes it.
            let c = cycles
                .into_iter()
                .filter_map(|members| {
                    members
                        .first()
                        .map(|&start| build_cycle(&graph, members, start, module_files))
                })
                .collect();

            Err(Error::CycleDetected(c))
        }
    }

    /// Given a package name, a set of existing interfaces and a checker function,
    /// check each module in its dependencies order.
    ///
    /// Every module is checked regardless of whether an earlier one failed: the
    /// successes are returned alongside the errors rather than being discarded as
    /// soon as one module fails (`BUG-2`). A successfully checked module still has
    /// its `Interface` inserted into `interfaces` as it goes, so later modules keep
    /// resolving against earlier ones even when some sibling failed.
    ///
    /// This is the narrower half of a wider idea, still open: making partial
    /// progress *within* one failing module (accumulating more errors from it
    /// rather than stopping at its first) instead of only across modules as this
    /// does. That would need `check` itself to return something like
    /// `Result<(Module, Errors), Errors>`, and scoped-fail semantics through
    /// canonicalization to produce it.
    ///
    /// `module_files` is how a checked module's `Interface` learns the file it was
    /// read from (`Interface::file`, `ERR-5`) — `check` itself is a phase-orchestrating
    /// function and, like every phase, never knows it. This method is driver code
    /// rather than a phase, the same as `compile_package`, so it is the one that can
    /// look the id up and stamp it on before the interface goes into the shared map.
    /// A module missing from `module_files` — there is none in the real pipeline,
    /// since only a module that already parsed reaches here — simply leaves that
    /// interface's `file` as `None`, same as a hand-built one.
    #[allow(clippy::type_complexity)]
    pub fn check_in_order<E>(
        &self,
        package: &crate::compiler::PackageName,
        interfaces: &mut HashMap<Name, crate::compiler::Interface>,
        module_files: &HashMap<Name, crate::compiler::source::files::SourceFileId>,
        check: fn(
            package: &crate::compiler::PackageName,
            interfaces: &HashMap<Name, crate::compiler::Interface>,
            source: &crate::compiler::parser::Module,
        ) -> Result<super::canonical::Module, E>,
    ) -> (Vec<crate::compiler::canonical::Module>, Vec<E>) {
        let mut modules = Vec::new();
        let mut errors = Vec::new();

        for module in self.modules.iter() {
            match check(package, interfaces, module) {
                Ok(m) => {
                    // Once we have successfuly checked a module, we can add it to the available interfaces
                    // for the following modules.
                    let iface_name = m.name.name().clone();
                    // Driver code, so this is where the module's file is known: the
                    // interface carries it so a *later* module's diagnostic can point
                    // back into this one's source (`ERR-5`).
                    let iface = m.to_interface(module_files.get(&module.name).copied());
                    debug!("Inserting {} with value {:?}", iface_name, iface);
                    interfaces.insert(iface_name, iface);

                    modules.push(m);
                }
                Err(err) => errors.push(err),
            }
        }

        (modules, errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::{Exposing, Import};
    use crate::compiler::position::NodeSpan;
    use crate::compiler::{canonical, parser, Interface, ModuleName, Name, PackageName};

    fn module<S: Into<String>>(name: S, deps: Vec<S>) -> Module {
        let imports = deps
            .into_iter()
            .map(|n| Import {
                name: Name::new(n),
                alias: None,
                exposing: Exposing::Open,
                // Hand-built, not parsed: there is no source text behind it.
                span: NodeSpan::none(),
            })
            .collect();

        Module {
            name: Name::new(name),
            binding_foreign: false,
            exposing: Exposing::Open,
            imports,
            infixes: vec![],
            types: vec![],
            functions: vec![],
        }
    }

    fn name<S: Into<String>>(s: S) -> Name {
        Name::new(s)
    }

    /// A `CycleEdge` matching what `module()` above produces: no span, since
    /// there is no source text behind a hand-built import, and no file, since
    /// these tests pass an empty `module_files` map to `ModuleWalker::new`.
    fn edge<S: Into<String>>(from: S, to: S) -> CycleEdge {
        CycleEdge {
            from: Name::new(from),
            to: Name::new(to),
            span: NodeSpan::none(),
            file: None,
        }
    }

    /// An empty canonical module standing in for whatever the real checker would
    /// have produced. Shared by the checkers below, which differ only in which
    /// modules they refuse.
    fn dummy_module(package: &PackageName, source: &parser::Module) -> canonical::Module {
        canonical::Module {
            name: ModuleName::new(package.clone(), source.name.clone()),
            exports: canonical::Exports::Everything,
            infixes: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
            binding_foreign: false,
        }
    }

    fn dummy_check(
        package: &PackageName,
        _interfaces: &HashMap<Name, Interface>,
        source: &parser::Module,
    ) -> Result<canonical::Module, ()> {
        Ok(dummy_module(package, source))
    }

    /// Like `dummy_check`, except module `b` always fails. Used to pin `BUG-2`:
    /// `check_in_order` must keep checking (and reporting) every other module
    /// rather than discarding them because one sibling failed.
    ///
    /// The error carries the failing module's name rather than being `()` so the
    /// test can assert *which* module failed; with `E = ()` the type system already
    /// guarantees the contents and only the arity would be under test.
    fn dummy_check_fails_for_b(
        package: &PackageName,
        _interfaces: &HashMap<Name, Interface>,
        source: &parser::Module,
    ) -> Result<canonical::Module, Name> {
        if source.name.as_str() == "b" {
            Err(source.name.clone())
        } else {
            Ok(dummy_module(package, source))
        }
    }

    fn assert_walker_processed_order(walker: ModuleWalker, expected: Vec<&str>) {
        let name = crate::compiler::PackageName::new("author", "project");
        let mut ifaces = HashMap::new();
        let module_files = HashMap::new();
        let (modules, errors): (Vec<canonical::Module>, Vec<()>) =
            walker.check_in_order(&name, &mut ifaces, &module_files, dummy_check);

        assert_eq!(errors, Vec::new());
        assert_eq!(
            modules
                .into_iter()
                .map(|m| m.name.name().as_str().to_string())
                .collect::<Vec<_>>(),
            expected
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn dependencies_without_cycle() {
        let a = module("a", vec![]);
        let b = module("b", vec!["a"]);
        let c = module("c", vec!["b"]);
        let d = module("d", vec!["a"]);

        let modules = vec![a, b, c, d];
        let module_files = HashMap::new();

        let walker = ModuleWalker::new(&modules, &module_files);

        assert_walker_processed_order(walker.expect("no errors here"), vec!["a", "b", "c", "d"])
    }

    /// The property `cycle_walk` is supposed to guarantee, asserted directly against
    /// the source modules rather than against `path` restated as `edges`: every
    /// consecutive pair in `path`, wrapping around from the last back to the first,
    /// is an `import` actually written in the earlier module.
    ///
    /// Checking `Cycle::edges` instead would pin nothing — `build_cycle` derives
    /// `from`/`to` from consecutive `path` entries unconditionally, and the two
    /// fields that could tell a real edge from a fabricated one are both dead in
    /// these fixtures (`file` is always `None` with an empty `module_files`, and
    /// `NodeSpan`'s `PartialEq` always returns `true`, per CLAUDE.md).
    fn assert_path_is_a_real_cycle(cycle: &Cycle, modules: &[Module]) {
        assert!(
            cycle.path.len() > 1,
            "a cycle between modules needs at least two of them, got {:?}",
            cycle.path
        );

        for (i, from) in cycle.path.iter().enumerate() {
            let to = &cycle.path[(i + 1) % cycle.path.len()];
            let from_module = modules
                .iter()
                .find(|m| &m.name == from)
                .unwrap_or_else(|| panic!("`{}` is one of the fixture modules", from));

            assert!(
                from_module.imports.iter().any(|imp| &imp.name == to),
                "`{}` does not import `{}`, so the reported cycle {:?} is not a walk \
                 this graph's edges can produce",
                from,
                to,
                cycle.path
            );
        }
    }

    /// Pins two things together: which modules a cycle names (as before this
    /// ticket), and — new for `ERR-6` — that its `path` is a genuine walk along
    /// real `import` edges rather than `tarjan_scc`'s raw, edge-agnostic
    /// component order. Before this change the note for the first cycle here
    /// printed `b -> c -> a -> b`, which is not a walk this graph's edges (`a ->
    /// c`, `b -> a`, `c -> b`) can produce — `b -> c` is not an edge. `path` is
    /// now `[b, a, c]`, which is: `b -> a` (b imports a), `a -> c` (a imports c),
    /// `c -> b` (c imports b, closing the loop).
    ///
    /// The hardcoded `path`/`edges` comparison pins *this* graph's answer; the
    /// `assert_path_is_a_real_cycle` calls pin the general property, and are what
    /// would catch a walk that closed its loop on an edge that does not exist. Both
    /// components here are simple cycles covering every member, so `others` is
    /// empty — `cycle_reports_component_members_left_off_the_loop` covers the case
    /// where it is not.
    ///
    /// Mutation-checked by reverting `cycle_walk` to return `members` verbatim
    /// (`tarjan_scc`'s raw order) instead of searching it: this test goes red
    /// (`[b, c, a]` instead of `[b, a, c]`, and `b` does not import `c`).
    /// `dependency_cycle_labels_each_import` in `tests/pipeline.rs` stays green
    /// under that same mutation and says so itself — its two-module fixture has only
    /// one possible walk, so the raw component order and a real walk coincide.
    #[test]
    fn dependencies_with_two_cycles() {
        let a = module("a", vec!["c"]);
        let b = module("b", vec!["a"]);
        let c = module("c", vec!["b"]);

        let d = module("d", vec!["a", "f"]);
        let e = module("e", vec!["d"]);
        let f = module("f", vec!["e"]);

        let modules = vec![a, b, c, d, e, f];
        let module_files = HashMap::new();

        let walker = ModuleWalker::new(&modules, &module_files);

        let res = walker.expect_err("I'm expecting an error");

        let Error::CycleDetected(cycles) = &res;
        for cycle in cycles {
            assert_path_is_a_real_cycle(cycle, &modules);
        }

        assert_eq!(
            res,
            Error::CycleDetected(vec![
                Cycle {
                    path: vec![name("b"), name("a"), name("c")],
                    others: vec![],
                    edges: vec![edge("b", "a"), edge("a", "c"), edge("c", "b")],
                },
                Cycle {
                    path: vec![name("e"), name("d"), name("f")],
                    others: vec![],
                    edges: vec![edge("e", "d"), edge("d", "f"), edge("f", "e")],
                },
            ])
        )
    }

    /// Every ordering of `specs` — a list of `(module name, its imports)` — handed to
    /// `ModuleWalker::new` in turn, with `assert` run on each resulting `Cycle`.
    ///
    /// The declaration order is what decides `tarjan_scc`'s ordering of a component,
    /// and therefore which member `build_cycle` starts its search from. A fixture
    /// written in one fixed order only ever exercises one start node, and a walk that
    /// is wrong from *some* start nodes will pass it by luck — which is exactly how
    /// the dead-end below went unnoticed. Running every permutation removes the luck.
    fn for_every_declaration_order(
        specs: &[(&str, Vec<&str>)],
        assert: impl Fn(&Cycle, &[Module], &[&str]),
    ) {
        // Lexicographic permutations of the indices into `specs`, so the whole set is
        // covered without pulling in a crate for it.
        let mut order: Vec<usize> = (0..specs.len()).collect();

        loop {
            let modules: Vec<Module> = order
                .iter()
                .map(|&i| module(specs[i].0, specs[i].1.clone()))
                .collect();
            let names: Vec<&str> = order.iter().map(|&i| specs[i].0).collect();
            let module_files = HashMap::new();

            let res = ModuleWalker::new(&modules, &module_files)
                .expect_err("every fixture here has a cycle in it");

            let Error::CycleDetected(cycles) = &res;
            assert_eq!(
                cycles.len(),
                1,
                "one component, so one cycle, in declaration order {:?}: {:?}",
                names,
                cycles
            );

            assert(&cycles[0], &modules, &names);

            // Next permutation.
            let Some(pivot) = (0..order.len() - 1)
                .rev()
                .find(|&i| order[i] < order[i + 1])
            else {
                break;
            };
            let successor = (pivot + 1..order.len())
                .rev()
                .find(|&i| order[i] > order[pivot])
                .expect("`pivot` was chosen because `pivot + 1` qualifies");
            order.swap(pivot, successor);
            order[pivot + 1..].reverse();
        }
    }

    /// The counterexample that sank the original greedy walk: a component holding
    /// two loops that share a node.
    ///
    /// ```text
    /// b imports c, e      b -> c -> d -> b
    /// c imports d         b -> e -> b
    /// d imports b
    /// e imports b
    /// ```
    ///
    /// `{b, c, d, e}` is one component. The greedy walk started at whichever member
    /// `tarjan_scc` listed first, took the first *unvisited* in-component neighbour
    /// at each step, and could walk itself into a corner: starting from `d` it went
    /// to `b`, from `b` to `e`, and `e`'s only neighbour `b` was already visited — so
    /// it stopped and returned `[d, b, e]`, whose wraparound `e -> d` is not an edge
    /// of this graph at all. The note then claimed `cycle: d -> b -> e -> d`, stating
    /// something false about the user's source, and `build_cycle` found no `import`
    /// behind that step so its label was silently dropped.
    ///
    /// Whether the start node is `d` depends on the declaration order, hence
    /// `for_every_declaration_order`.
    ///
    /// Mutation-checked against exactly that: restoring the greedy walk turns this
    /// test red on `e` not importing `d`.
    #[test]
    fn cycle_closes_on_a_real_import_when_two_loops_share_a_module() {
        let specs = [
            ("b", vec!["c", "e"]),
            ("c", vec!["d"]),
            ("d", vec!["b"]),
            ("e", vec!["b"]),
        ];

        for_every_declaration_order(&specs, |cycle, modules, order| {
            assert_path_is_a_real_cycle(cycle, modules);
            // Guard against the property being satisfied vacuously by a walk that
            // stopped one step in.
            assert!(
                cycle.path.len() >= 2,
                "declaration order {:?} produced {:?}",
                order,
                cycle.path
            );
        });
    }

    /// A component whose loops do not all share their members: the reported `path`
    /// is necessarily a strict subset of it.
    ///
    /// ```text
    /// a imports b         a -> b -> d -> a
    /// b imports d, c      b -> c -> b
    /// c imports b
    /// d imports a
    /// ```
    ///
    /// `{a, b, c, d}` is one component holding two loops that overlap only at `b`.
    /// No cycle covers all four, so whichever one `cycle_walk` reports leaves
    /// members out — and a user who breaks only the loop they were shown walks
    /// straight into the other. `Cycle::others` is how the ones left out stay
    /// visible, and `notes()` renders them.
    ///
    /// Mutation-checked by hardcoding `others: vec![]` in `build_cycle`: the union
    /// assertion below goes red.
    #[test]
    fn cycle_reports_component_members_left_off_the_loop() {
        use crate::compiler::PhaseError;

        let specs = [
            ("a", vec!["b"]),
            ("b", vec!["d", "c"]),
            ("c", vec!["b"]),
            ("d", vec!["a"]),
        ];

        for_every_declaration_order(&specs, |cycle, modules, order| {
            // Whichever loop was picked, it has to be a real one…
            assert_path_is_a_real_cycle(cycle, modules);

            // …and no member of the component may go unmentioned.
            let mut named: Vec<Name> = cycle
                .path
                .iter()
                .chain(cycle.others.iter())
                .cloned()
                .collect();
            named.sort_by(|l, r| l.as_str().cmp(r.as_str()));
            assert_eq!(
                named,
                vec![name("a"), name("b"), name("c"), name("d")],
                "`path` plus `others` must name the whole component; declaration \
                 order {:?} gave path {:?} and others {:?}",
                order,
                cycle.path,
                cycle.others
            );

            // No cycle here covers all four, so something is always left out — and
            // the note, not just the struct, has to say what.
            assert!(
                !cycle.others.is_empty(),
                "no loop in this component covers it, so `others` cannot be empty; \
                 declaration order {:?} gave path {:?}",
                order,
                cycle.path
            );

            let notes = Error::CycleDetected(vec![cycle.clone()]).notes();
            assert!(
                notes.iter().any(|n| {
                    n.starts_with("also part of this circular dependency")
                        && cycle.others.iter().all(|o| n.contains(o.as_str()))
                }),
                "expected a note naming the members left off the loop, got {:?}",
                notes
            );
        });
    }

    /// `LANG-8`: a module that never wrote `import Basics` is still checked after
    /// `Basics`, because an implicit import can only resolve against an
    /// `Interface` that already exists.
    ///
    /// `Main` is declared *first*, which is what makes this discriminating: with
    /// no edge between the two, `tarjan_scc` hands back the isolated nodes in
    /// declaration order and `Main` would be checked first. Mutation-checked by
    /// dropping the `add_default_import_edges` call in `ModuleWalker::new`, which
    /// flips the expected order.
    #[test]
    fn a_default_import_orders_the_module_it_names_first() {
        let main = module("Main", vec![]);
        let basics = module("Basics", vec![]);

        let modules = vec![main, basics];
        let module_files = HashMap::new();

        let walker = ModuleWalker::new(&modules, &module_files).expect("no cycle here");

        assert_walker_processed_order(walker, vec!["Basics", "Main"]);
    }

    /// `LANG-8`: the modules a default import is *built from* do not receive it.
    ///
    /// This is `std/core`'s own shape — `Basics` imports the `Js.Basics` facade —
    /// and an unconditional implicit edge from `Js.Basics` back to `Basics` would
    /// make the standard library a dependency cycle. `add_default_import_edges`
    /// drops an edge whose target already depends on the importer, so the written
    /// direction is the only one and `Js.Basics` is checked first.
    ///
    /// Mutation-checked by dropping the `has_path_connecting` guard: this becomes
    /// `Err(CycleDetected(..))` and `expect` panics.
    #[test]
    fn a_default_import_does_not_close_a_loop_onto_its_own_dependency() {
        let basics = module("Basics", vec!["Js.Basics"]);
        let js_basics = module("Js.Basics", vec![]);

        let modules = vec![basics, js_basics];
        let module_files = HashMap::new();

        let walker = ModuleWalker::new(&modules, &module_files)
            .expect("the standard library's own shape is not a cycle");

        assert_walker_processed_order(walker, vec!["Js.Basics", "Basics"]);
    }

    #[test]
    fn dependencies_with_branches() {
        /*
        a <- c <- b <- d
          <- e <- f <- g <- h

        i -> b
          -> e
          <- h
        i should be after e, c, and b; h should be after i; no guarantee on the order when there is no constraint
        */
        let a = module("a", vec![]);
        let b = module("c", vec!["a"]);
        let c = module("b", vec!["c"]);

        let d = module("d", vec!["c"]);
        let e = module("e", vec!["a"]);
        let f = module("f", vec!["e"]);
        let g = module("g", vec!["f"]);
        let h = module("h", vec!["g", "i"]);

        let i = module("i", vec!["b", "e"]);

        let modules = vec![a, c, b, d, e, f, g, h, i];
        let module_files = HashMap::new();

        let walker = ModuleWalker::new(&modules, &module_files);

        assert_walker_processed_order(
            walker.expect("no errors here"),
            vec!["a", "c", "b", "d", "e", "f", "g", "i", "h"],
        )
    }

    /// `BUG-2`: one failing module used to make `check_in_order` discard every
    /// module that checked successfully, returning `Err(errors)` with no way to
    /// recover the successes. It must now hand back both: the modules that
    /// checked, and the errors from the ones that didn't.
    ///
    /// Mutation-checked with `modules.clear()` before the `(modules, errors)` return
    /// below, guarded on `!errors.is_empty()` — the old discard behaviour, expressed
    /// in a way that still compiles against the tuple return type. It turns this test
    /// red, since `successes` then comes back empty instead of `["a", "c"]`.
    /// (Literally restoring `collect_accumulate` would not be rerunnable: it changes
    /// the return type back to `Result<Vec<Module>, Vec<E>>`, so the destructuring
    /// below stops typechecking and the test fails to build rather than to assert.)
    #[test]
    fn check_in_order_keeps_successful_modules_when_one_fails() {
        let a = module("a", vec![]);
        let b = module("b", vec!["a"]);
        let c = module("c", vec!["b"]);

        let modules = vec![a, b, c];
        let module_files = HashMap::new();
        let walker = ModuleWalker::new(&modules, &module_files).expect("no errors here");

        let name = crate::compiler::PackageName::new("author", "project");
        let mut ifaces = HashMap::new();
        let (successes, errors) =
            walker.check_in_order(&name, &mut ifaces, &module_files, dummy_check_fails_for_b);

        assert_eq!(
            errors,
            vec![Name::new("b")],
            "expected exactly the one error, and for `b`"
        );
        assert_eq!(
            successes
                .into_iter()
                .map(|m| m.name.name().as_str().to_string())
                .collect::<Vec<_>>(),
            vec!["a".to_string(), "c".to_string()],
            "expected the non-failing modules to still be returned"
        );
    }

    /// The implicit edges `add_default_import_edges` adds, as slot pairs.
    ///
    /// Slots rather than names, so that two namings of one package shape produce
    /// directly comparable answers.
    fn implicit_edges(names: &[&str], written: &[Vec<usize>]) -> Vec<(usize, usize)> {
        let modules: Vec<Module> = names
            .iter()
            .enumerate()
            .map(|(i, n)| {
                let deps: Vec<&str> = written[i].iter().map(|&j| names[j]).collect();
                module(*n, deps)
            })
            .collect();

        let mut graph = DiGraph::new();
        let mut idx_of = HashMap::new();
        let mut slot_of = HashMap::new();
        for (slot, m) in modules.iter().enumerate() {
            let idx = graph.add_node(m);
            idx_of.insert(&m.name, idx);
            slot_of.insert(idx, slot);
        }
        for m in modules.iter() {
            let from = idx_of[&m.name];
            for &to in m.imports.iter().filter_map(|i| idx_of.get(&i.name)) {
                graph.add_edge(from, to, ());
            }
        }

        let before = graph.edge_count();
        add_default_import_edges(&mut graph, &idx_of);

        let mut added: Vec<(usize, usize)> = graph
            .edge_indices()
            .skip(before)
            .filter_map(|e| graph.edge_endpoints(e))
            .map(|(from, to)| (slot_of[&from], slot_of[&to]))
            .collect();
        added.sort_unstable();
        added
    }

    /// `LANG-8`: which default imports a module ends up with is a property of the
    /// package's shape, not of what its modules are called.
    ///
    /// `add_default_import_edges` refuses an edge against the graph *as built so
    /// far*, so the edges it has already added constrain the ones it may still add.
    /// Nesting the loops importer-first made that self-interference follow the
    /// alphabet — `tests/fixtures/package_default_import_priority/` is a four-module
    /// package that compiled or did not depending on one module's name. Nesting them
    /// target-first fixes the outcome; this pins that over shapes nobody wrote by
    /// hand.
    ///
    /// Each trial builds a random acyclic package — some modules named on
    /// `DEFAULT_IMPORTS`, the rest not — and then renames only the *non*-default
    /// modules, by rotating their names through the same slots. Renaming a default
    /// module would be a different package, so those names stay put.
    ///
    /// Mutation-checked by restoring the old `for importer { for default }` nesting:
    /// this fails inside the first twenty trials.
    #[test]
    fn renaming_a_module_does_not_change_which_default_imports_it_gets() {
        // A fixed-seed LCG rather than a dependency: the shapes have to be the same
        // ones on every run, or a failure here could not be reproduced.
        let mut seed: u64 = 0x243F_6A88_85A3_08D3;
        let mut rnd = move |n: usize| {
            seed = seed
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            (seed >> 33) as usize % n
        };

        let defaults = ["Basics", "Maybe", "Result", "Tuple", "List"];
        let pool = [
            "Aux", "Bee", "Cee", "Dee", "Eee", "Mmm", "Nnn", "Yak", "Zed",
        ];

        for trial in 0..2000 {
            let default_count = rnd(4);
            let other_count = 2 + rnd(5);
            let total = default_count + other_count;

            let mut default_names: Vec<&str> = Vec::new();
            while default_names.len() < default_count {
                let candidate = defaults[rnd(defaults.len())];
                if !default_names.contains(&candidate) {
                    default_names.push(candidate);
                }
            }
            let mut other_names: Vec<&str> = Vec::new();
            while other_names.len() < other_count {
                let candidate = pool[rnd(pool.len())];
                if !other_names.contains(&candidate) {
                    other_names.push(candidate);
                }
            }

            // Which slots hold a module named on the list.
            let mut is_default = vec![false; total];
            let mut remaining = default_count;
            for (slot, flag) in is_default.iter_mut().enumerate() {
                if remaining > 0 && (total - slot == remaining || rnd(2) == 0) {
                    *flag = true;
                    remaining -= 1;
                }
            }

            // Written imports, acyclic by construction: a slot only imports lower ones.
            let written: Vec<Vec<usize>> = (0..total)
                .map(|slot| (0..slot).filter(|_| rnd(3) == 0).collect())
                .collect();

            let naming = |others: &[&'static str]| -> Vec<&'static str> {
                let mut defaults = default_names.iter();
                let mut others = others.iter();
                (0..total)
                    .map(|slot| {
                        if is_default[slot] {
                            *defaults.next().unwrap()
                        } else {
                            *others.next().unwrap()
                        }
                    })
                    .collect()
            };

            let expected = implicit_edges(&naming(&other_names), &written);

            for shift in 1..other_names.len() {
                let mut renamed = other_names.clone();
                renamed.rotate_left(shift);

                assert_eq!(
                    implicit_edges(&naming(&renamed), &written),
                    expected,
                    "trial {} renamed {:?} to {:?}, written imports {:?}",
                    trial,
                    other_names,
                    renamed,
                    written
                );
            }
        }
    }
}
