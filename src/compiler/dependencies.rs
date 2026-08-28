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
use std::collections::{HashMap, HashSet};

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

/// A dependency cycle: the modules involved, in cycle order, alongside the
/// `import` that created each edge between consecutive modules (wrapping around
/// to the first).
#[derive(Debug, PartialEq, Clone)]
pub struct Cycle {
    /// The modules in cycle order — what the summary note is built from.
    pub path: Vec<Name>,
    /// `edges[i]` is the `import` written in `path[i]` that names `path[i + 1]`
    /// (or `path[0]`, for the last edge). Same length as `path`.
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
            // `A -> B -> A`.
            Error::CycleDetected(cycles) => cycles
                .iter()
                .map(|cycle| {
                    let mut path: Vec<String> =
                        cycle.path.iter().map(|name| name.to_string()).collect();
                    if let Some(first) = path.first().cloned() {
                        path.push(first);
                    }
                    format!("cycle: {}", path.join(" -> "))
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
                            message: format!(
                                "`{}` imports `{}` here, closing the cycle",
                                edge.from, edge.to
                            ),
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
/// node in it can reach every other — into an actual directed cycle: a sequence of
/// modules where each one really does import the next.
///
/// `tarjan_scc`'s own ordering of a component does not promise this: it is
/// membership, not a walk, so consecutive entries are not necessarily connected by
/// an edge. This does a small greedy DFS from `members[0]`, always preferring a
/// same-component edge that closes the loop back to the start, so `path`'s
/// consecutive pairs (and its last pair back to `path[0]`) are real edges in the
/// graph — which `build_cycle` then needs to look up the `import` behind each one.
///
/// This only fails to close the loop for a component with a more tangled shape
/// than a simple cycle (two overlapping loops sharing a node, say); every fixture
/// and every real import graph seen so far is a simple cycle, and `build_cycle`
/// degrades gracefully — a missing edge just yields a label-less `CycleEdge` for
/// that one step — if it ever doesn't.
fn cycle_walk(graph: &DiGraph<&Module, ()>, members: &[NodeIndex]) -> Vec<NodeIndex> {
    let member_set: HashSet<NodeIndex> = members.iter().copied().collect();
    let start = members[0];
    let mut path = vec![start];
    let mut visited: HashSet<NodeIndex> = HashSet::from([start]);
    let mut current = start;

    loop {
        let neighbors: Vec<NodeIndex> = graph
            .neighbors(current)
            .filter(|n| member_set.contains(n))
            .collect();

        // Once we've moved past the start, an edge back to it closes the cycle.
        if path.len() > 1 && neighbors.contains(&start) {
            break;
        }

        match neighbors.into_iter().find(|n| !visited.contains(n)) {
            Some(next) => {
                path.push(next);
                visited.insert(next);
                current = next;
            }
            // A dead end within the component: the greedy walk above couldn't
            // close the loop. See this function's own documentation.
            None => break,
        }
    }

    path
}

/// Turn one strongly-connected component into the [`Cycle`] a diagnostic renders:
/// the module path `cycle_walk` finds, plus the `import` behind each edge of it.
fn build_cycle(
    graph: &DiGraph<&Module, ()>,
    members: &[NodeIndex],
    module_files: &HashMap<Name, SourceFileId>,
) -> Cycle {
    let node_path = cycle_walk(graph, members);
    let path: Vec<Name> = node_path
        .iter()
        .map(|&idx| graph[idx].name.clone())
        .collect();

    let edges = node_path
        .iter()
        .enumerate()
        .map(|(i, &idx)| {
            let from_module = graph[idx];
            let to_idx = node_path[(i + 1) % node_path.len()];
            let to_name = graph[to_idx].name.clone();

            // The specific `import …` line in `from_module` that names `to_name` —
            // there may be several imports in `from_module`, but at most one of
            // them names this particular neighbour.
            let import = from_module.imports.iter().find(|imp| imp.name == to_name);

            CycleEdge {
                from: from_module.name.clone(),
                to: to_name,
                span: import.map(|imp| imp.span).unwrap_or_else(NodeSpan::none),
                file: import.and_then(|_| module_files.get(&from_module.name).copied()),
            }
        })
        .collect();

    Cycle { path, edges }
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

        // Find the strongly connected graphs (scc), if there are more than one node per scc
        // it means there is a circular dependency.
        let scc = petgraph::algo::tarjan_scc(&graph);

        let (cycles, deps): (Vec<_>, Vec<_>) = scc.iter().partition(|&v| v.len() > 1);

        if cycles.is_empty() {
            let modules = deps.into_iter().flatten().map(|&idx| graph[idx]).collect();

            Ok(ModuleWalker { modules })
        } else {
            let c = cycles
                .into_iter()
                .map(|members| build_cycle(&graph, members, module_files))
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
            binding_javascript: false,
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
            binding_javascript: false,
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

    /// Pins two things together: which modules a cycle names (as before this
    /// ticket), and — new for `ERR-6` — that its `path` is a genuine walk along
    /// real `import` edges rather than `tarjan_scc`'s raw, edge-agnostic
    /// component order. Before this change the note for the first cycle here
    /// printed `b -> c -> a -> b`, which is not a walk this graph's edges (`a ->
    /// c`, `b -> a`, `c -> b`) can produce — `b -> c` is not an edge. `path` is
    /// now `[b, a, c]`, which is: `b -> a` (b imports a), `a -> c` (a imports c),
    /// `c -> b` (c imports b, closing the loop).
    ///
    /// `edges` is asserted alongside `path`: each `CycleEdge::from`/`to` names the
    /// consecutive pair in `path` it corresponds to, which only holds if `path`
    /// really is edge-consecutive.
    ///
    /// Mutation-checked by reverting `cycle_walk` to return `members` verbatim
    /// (`tarjan_scc`'s raw order) instead of walking it — this test's `path`
    /// assertion goes red (`[b, c, a]` instead of `[b, a, c]`), and so does
    /// `dependency_cycle_labels_each_import` in `tests/pipeline.rs`, whose labels
    /// stop finding a matching `import` for the non-edge pairs and drop to zero.
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

        assert_eq!(
            res,
            Error::CycleDetected(vec![
                Cycle {
                    path: vec![name("b"), name("a"), name("c")],
                    edges: vec![edge("b", "a"), edge("a", "c"), edge("c", "b")],
                },
                Cycle {
                    path: vec![name("e"), name("d"), name("f")],
                    edges: vec![edge("e", "d"), edge("d", "f"), edge("f", "e")],
                },
            ])
        )
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
}
