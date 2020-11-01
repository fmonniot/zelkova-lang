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

    pub fn process<T, E>(&self, f: impl Fn(&'a Module) -> Result<T, E>) -> Result<Vec<T>, Vec<E>> {
        let iter = self.modules.iter().map(|module| f(module));

        collect_accumulate(iter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::{Exposing, Import};

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

    fn assert_walker_processed_order(walker: ModuleWalker, expected: Vec<&str>) {
        let res: Result<Vec<String>, Vec<()>> = walker.process(|m| Ok(m.name.0.clone()));

        assert_eq!(
            res,
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
