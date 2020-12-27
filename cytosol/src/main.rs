use std::path::PathBuf;

use clap::Clap;

#[derive(Debug, Clap)]
#[clap(version = "0.1", author = "Tia")]
struct Arguments {
    #[clap(long)]
    dump_tokens: bool,

    file_paths: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    if args.dump_tokens {
        dump_tokens(&args.file_paths)?;
    } else {
        // TODO
    }

    Ok(())
}

fn dump_tokens(paths: &[PathBuf]) -> std::io::Result<()> {
    for path in paths {
        let file = path.to_str();
        let content = std::fs::read_to_string(path)?;
        let toks = cytosol_parser::tokenise(file, &content);

        for (_, tok) in toks {
            println!("{:?}", tok);
        }
    }

    Ok(())
}
