use std::{
    collections::BTreeMap,
    io::Write,
    path::{Path, PathBuf},
    time::{Duration, Instant},
    writeln,
};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FileStage {
    Lexing,
    Parsing,
}

pub struct FileSummary {
    // could also be from a REPL or something.
    path: PathBuf,
    entries: BTreeMap<FileStage, Duration>,
}

impl FileSummary {
    pub fn new(path: &Path) -> Self {
        Self {
            path: path.to_path_buf(),
            entries: Default::default(),
        }
    }

    pub fn record<R>(&mut self, stage: FileStage, f: impl FnOnce() -> R) -> R {
        let start = Instant::now();
        let res = f();
        let end = Instant::now();
        self.entries.insert(stage, end.duration_since(start));
        res
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ProgramStage {
    AstToHir,
}

#[derive(Default)]
pub struct TimingReport {
    files: Vec<FileSummary>,
    entries: BTreeMap<ProgramStage, Duration>,
}

impl TimingReport {
    pub fn add_file(&mut self, file: FileSummary) {
        self.files.push(file);
    }

    pub fn record<R>(&mut self, stage: ProgramStage, f: impl FnOnce() -> R) -> R {
        let start = Instant::now();
        let res = f();
        let end = Instant::now();
        self.entries.insert(stage, end.duration_since(start));
        res
    }

    pub fn print_summary(&self, writer: &mut impl Write) -> std::io::Result<()> {
        writeln!(writer, "=========================")?;
        writeln!(writer, "Timing summary")?;
        writeln!(writer)?;

        for file in &self.files {
            if file.entries.is_empty() {
                continue;
            }
            writeln!(writer, "File summary: {}", file.path.display())?;

            for (stage, duration) in &file.entries {
                writeln!(
                    writer,
                    "  {:?} - {}",
                    stage,
                    humantime::Duration::from(*duration),
                )?;
            }
            writeln!(writer)?;
        }

        for (stage, duration) in &self.entries {
            writeln!(
                writer,
                "{:?} - {}",
                stage,
                humantime::Duration::from(*duration),
            )?;
        }

        Ok(())
    }
}
