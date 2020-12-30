pub mod driver;

mod debug;
mod reporting;

pub mod timing;

pub struct Config {
    /// Do not display colours in the terminal
    pub no_colour: bool,
}
