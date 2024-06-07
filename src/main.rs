use regorus::unstable::*;
use regorus::*;

mod ast;
mod parser;

use anyhow::Result;

fn main() -> Result<()> {
    let source = Source::from_file("policy.cedar")?;
    let mut parser = parser::Parser::new(&source)?;
    let policy = parser.parse()?;
    dbg!(&policy);
    Ok(())
}
