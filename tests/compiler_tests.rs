mod compiler {
    mod parser {
        #[macro_use]
        mod support;

        mod expressions;
        mod layout;
        mod modules;
        mod recovery;
        mod types;
    }

    mod canonical;
}
