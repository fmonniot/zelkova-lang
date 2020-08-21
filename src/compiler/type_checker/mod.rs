//! This module contains the type checker pass of the language
//! 
//! It works with the result of the frontend and will perform two jobs:
//! - type checks the different declarations and expression
//! - infer the types when not declared in the source
//! 
//! I have no idea how that works; so bear with me while I explore
//! the space, make mistake and (hopefully) learn something :)
//! 
//! Some papers on type inference:
//! - http://steshaw.org/hm/hindley-milner.pdf
//! - https://pdfs.semanticscholar.org/8983/233b3dff2c5b94efb31235f62bddc22dc899.pdf
//! - http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf
//! 
