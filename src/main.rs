use clap::{Parser, Subcommand};
use loxr::value::Function;
use loxr::{Chunk, OpCode, Value, Vm};
use miette::{IntoDiagnostic, WrapErr};
use std::fs;
use std::io::Write;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
    Interpret { filename: PathBuf },
    Debug { filename: PathBuf },
    DebugChunk { filename: PathBuf },
    Chunk,
    Repl { debug: Option<bool> },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::DebugChunk { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let mut vm = Vm::new();
            vm.compile(&file_contents)?;

            println!("{}", vm.code());
            Ok(())
        }
        Commands::Interpret { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let mut vm = Vm::new();
            vm.compile(&file_contents)?;
            vm.run()?;
            Ok(())
        }

        Commands::Debug { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let mut vm = Vm::new().with_debug();
            vm.compile(&file_contents)?;
            println!("{}", vm.code());
            vm.run()?;
            Ok(())
        }
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            for token in loxr::Lexer::new(&file_contents) {
                let token = token?;
                println!("{token}");
            }
            println!("EOF  null");
            Ok(())
        }
        Commands::Chunk => {
            let mut fun = Function::new();
            fun.code.push_const(Value::Number(1.));
            fun.code.push_const(Value::Number(5.));
            fun.code.push_opcode(OpCode::OpAdd);
            fun.code.push_const(Value::Number(6.));
            fun.code.push_opcode(OpCode::OpMultiply);
            fun.code.push_opcode(OpCode::OpReturn);

            println!("{}", fun.code);

            let mut vm = Vm::with_fun(fun).with_debug();
            vm.run()?;
            Ok(())
        }
        Commands::Repl { debug } => {
            let mut line = String::new();
            let mut vm = if debug.is_some_and(|x| x) {
                Vm::new().with_debug()
            } else {
                Vm::new()
            };
            loop {
                print!("> ");
                let _ = std::io::stdout().flush();
                let _ = std::io::stdin()
                    .read_line(&mut line)
                    .expect("Expected input line");
                vm.compile(&line)?;
                vm.run()?;
                line.clear();
            }
        }
    }
}
