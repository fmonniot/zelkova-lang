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
use crate::utils::collect_accumulate;
use log::debug;
use std::collections::HashMap;

use petgraph::graph::DiGraph;

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

#[derive(Debug, PartialEq)]
pub enum Error {
    CycleDetected(Vec<Vec<Name>>),
}

impl<'a> ModuleWalker<'a> {
    pub fn new(modules: &'a Vec<Module>) -> Result<ModuleWalker, Error> {
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
                .map(|cycle| {
                    cycle
                        .into_iter()
                        .map(|&idx| graph[idx].name.clone())
                        .collect()
                })
                .collect();

            Err(Error::CycleDetected(c))
        }
    }

    /// Given a package name, a set of existing interfaces and a checker function,
    /// Check each module in its dependencies order
    pub fn check_in_order<E>(
        &self,
        package: &crate::compiler::PackageName,
        interfaces: &mut HashMap<Name, crate::compiler::Interface>,
        check: fn(
            package: &crate::compiler::PackageName,
            interfaces: &HashMap<Name, crate::compiler::Interface>,
            source: &crate::compiler::parser::Module,
        ) -> Result<super::canonical::Module, E>,
    ) -> Result<Vec<crate::compiler::canonical::Module>, Vec<E>> {
        let iter = self
            .modules
            .iter()
            .map(|module| match check(package, interfaces, module) {
                Ok(m) => {
                    // Once we have successfuly checked a module, we can add it to the available interfaces
                    // for the following modules.
                    // TODO We might want to have a less strict approach if we want to make some progress
                    // in dependent modules even if the current one doesn't pass all checks (accumulate
                    // more errors to show at once to the programmer).
                    // In term of types, it means having check return something like Result<(Module, Errors), Errors>.
                    // The overall Result is for the errors stopping us from building the minimum required to make
                    // a Module. This might not be required if we decide how to default the Exports (the rest is vec based,
                    // so can be default to ignore). This also means the whole canonicalization process will have to be
                    // rewritten to have scoped-fail semantics.
                    let iface_name = m.name.name().clone();
                    let iface = m.to_interface();
                    debug!("Inserting {} with value {:?}", iface_name, iface);
                    interfaces.insert(iface_name, iface);

                    Ok(m)
                }
                Err(err) => Err(err),
            });

        collect_accumulate(iter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::{Exposing, Import};
    use crate::compiler::{canonical, parser, Interface, ModuleName, Name, PackageName};

    fn module<S: Into<String>>(name: S, deps: Vec<S>) -> Module {
        let imports = deps
            .into_iter()
            .map(|n| Import {
                name: Name(n.into()),
                alias: None,
                exposing: Exposing::Open,
            })
            .collect();

        Module {
            name: Name(name.into()),
            binding_javascript: false,
            exposing: Exposing::Open,
            imports,
            infixes: vec![],
            types: vec![],
            functions: vec![],
        }
    }

    fn name<S: Into<String>>(s: S) -> Name {
        Name(s.into())
    }

    fn dummy_check(
        package: &PackageName,
        _interfaces: &HashMap<Name, Interface>,
        source: &parser::Module,
    ) -> Result<canonical::Module, ()> {
        Ok(canonical::Module {
            name: ModuleName::new(package.clone(), source.name.clone()),
            exports: canonical::Exports::Everything,
            infixes: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
        })
    }

    fn assert_walker_processed_order(walker: ModuleWalker, expected: Vec<&str>) {
        let name = crate::compiler::PackageName::new("author", "project");
        let mut ifaces = HashMap::new();
        let res: Result<Vec<canonical::Module>, Vec<()>> =
            walker.check_in_order(&name, &mut ifaces, dummy_check);

        assert_eq!(
            res.map(|v| v
                .into_iter()
                .map(|m| m.name.name().0.clone())
                .collect::<Vec<_>>()),
            Ok(expected.into_iter().map(|s| s.to_string()).collect())
        );
    }

    #[test]
    fn dependencies_without_cycle() {
        let a = module("a", vec![]);
        let b = module("b", vec!["a"]);
        let c = module("c", vec!["b"]);
        let d = module("d", vec!["a"]);

        let modules = vec![a, b, c, d];

        let walker = ModuleWalker::new(&modules);

        assert_walker_processed_order(walker.expect("no errors here"), vec!["a", "b", "c", "d"])
    }

    #[test]
    fn dependencies_with_two_cycles() {
        let a = module("a", vec!["c"]);
        let b = module("b", vec!["a"]);
        let c = module("c", vec!["b"]);

        let d = module("d", vec!["a", "f"]);
        let e = module("e", vec!["d"]);
        let f = module("f", vec!["e"]);

        let modules = vec![a, b, c, d, e, f];

        let walker = ModuleWalker::new(&modules);

        let res = walker.expect_err("I'm expecting an error");

        assert_eq!(
            res,
            Error::CycleDetected(vec![
                vec![name("b"), name("c"), name("a")],
                vec![name("e"), name("f"), name("d")]
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

        let walker = ModuleWalker::new(&modules);

        assert_walker_processed_order(
            walker.expect("no errors here"),
            vec!["a", "c", "b", "d", "e", "f", "g", "i", "h"],
        )
    }
}
