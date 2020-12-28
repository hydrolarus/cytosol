use std::path::PathBuf;

use clap::Clap;
use cytosol_syntax::pretty_print;

#[derive(Debug, Clap)]
#[clap(version = "0.1", author = "Tia")]
struct Arguments {
    #[clap(long)]
    dump_tokens: bool,

    #[clap(long)]
    dump_ast: bool,

    file_paths: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    if args.dump_tokens {
        dump_tokens(&args.file_paths)?;
    } else if args.dump_ast {
        dump_ast(&args.file_paths)?;
    } else {
        // TODO
    }

    Ok(())
}

fn dump_ast(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = codespan::Files::new();

    for path in paths {
        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.file_name().unwrap(), content);

        let toks = cytosol_parser::tokenise(id, files.source(id)).collect::<Vec<_>>();

        let ast = cytosol_parser::parse_file(&toks)?;

        println!("{}", pretty_print(&ast, 80));
    }

    Ok(())
}

fn dump_tokens(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = codespan::Files::new();

    for path in paths {
        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.file_name().unwrap(), content);

        let toks = cytosol_parser::tokenise(id, files.source(id));

        for tok in toks {
            println!("{:?}", tok.kind);
        }
    }

    Ok(())
}
