use std::{
    collections::{btree_map::Entry, BTreeMap},
    io::Write,
    time::{Duration, Instant},
    writeln,
};

use comfy_table::Table;
use stats_alloc::Stats;

use cytosol::driver::FileName;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FileStage {
    Lexing,
    Parsing,
}

#[derive(Clone)]
struct Measurement {
    time: Duration,
    allocation_stats: Stats,
}

impl Measurement {
    fn sum(&self, other: &Self) -> Self {
        Self {
            time: self.time + other.time,
            allocation_stats: Stats {
                allocations: self.allocation_stats.allocations + other.allocation_stats.allocations,
                deallocations: self.allocation_stats.deallocations
                    + other.allocation_stats.deallocations,
                reallocations: self.allocation_stats.reallocations
                    + other.allocation_stats.reallocations,
                bytes_allocated: self.allocation_stats.bytes_allocated
                    + other.allocation_stats.bytes_allocated,
                bytes_deallocated: self.allocation_stats.bytes_deallocated
                    + other.allocation_stats.bytes_deallocated,
                bytes_reallocated: self.allocation_stats.bytes_reallocated
                    + other.allocation_stats.bytes_reallocated,
            },
        }
    }

    fn display_columns(&self) -> (String, [String; 3]) {
        fn display_alloc_info(num: usize, bytes: isize) -> String {
            use humansize::FileSize;

            format!(
                "{} ({})",
                num,
                bytes
                    .file_size(humansize::file_size_opts::CONVENTIONAL)
                    .unwrap()
            )
        }

        (
            humantime::Duration::from(self.time).to_string(),
            [
                display_alloc_info(
                    self.allocation_stats.allocations,
                    self.allocation_stats.bytes_allocated as _,
                ),
                display_alloc_info(
                    self.allocation_stats.reallocations,
                    self.allocation_stats.bytes_reallocated,
                ),
                display_alloc_info(
                    self.allocation_stats.deallocations,
                    self.allocation_stats.bytes_deallocated as _,
                ),
            ],
        )
    }
}

pub struct FileSummary {
    // could also be from a REPL or something.
    name: FileName,
    entries: BTreeMap<FileStage, Measurement>,
}

impl FileSummary {
    pub fn new(name: &FileName) -> Self {
        Self {
            name: name.clone(),
            entries: Default::default(),
        }
    }

    pub fn record<R>(&mut self, stage: FileStage, f: impl FnOnce() -> R) -> R {
        let stats_before = crate::INSTRUMENTED_SYSTEM.stats();
        let start = Instant::now();
        let res = f();
        let end = Instant::now();
        let stats_after = crate::INSTRUMENTED_SYSTEM.stats();
        let time = end.duration_since(start);
        let allocation_stats = stats_after - stats_before;
        self.entries.insert(
            stage,
            Measurement {
                time,
                allocation_stats,
            },
        );
        res
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ProgramStage {
    AstToHir,
    Execution,
}

#[derive(Default)]
pub struct PerformanceReport {
    files: Vec<FileSummary>,
    entries: BTreeMap<ProgramStage, Measurement>,
}

impl PerformanceReport {
    pub fn add_file(&mut self, file: FileSummary) {
        self.files.push(file);
    }

    pub fn record<R>(&mut self, stage: ProgramStage, f: impl FnOnce() -> R) -> R {
        let stats_before = crate::INSTRUMENTED_SYSTEM.stats();
        let start = Instant::now();
        let res = f();
        let end = Instant::now();
        let stats_after = crate::INSTRUMENTED_SYSTEM.stats();
        let time = end.duration_since(start);
        let allocation_stats = stats_after - stats_before;

        let measurement = Measurement {
            time,
            allocation_stats,
        };

        match self.entries.entry(stage) {
            Entry::Vacant(e) => {
                e.insert(measurement);
            }
            Entry::Occupied(mut e) => {
                let new = e.get().sum(&measurement);
                *e.get_mut() = new;
            }
        }
        res
    }

    pub fn print_perf_report(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let mut table = Table::new();
        table.load_preset(TABLE_PRESET);
        table.set_header(vec!["Stage", "Time", "alloc", "realloc", "dealloc"]);

        let mut lex_parse = BTreeMap::<FileStage, Measurement>::new();

        for file in &self.files {
            for (s, m) in &file.entries {
                lex_parse
                    .entry(*s)
                    .and_modify(|e| *e = e.sum(m))
                    .or_insert_with(|| m.clone());
            }
        }

        for (s, m) in lex_parse {
            let (time, [a, r, d]) = m.display_columns();
            table.add_row(vec![format!("{:?}", s), time, a, r, d]);
        }

        for (s, m) in &self.entries {
            let (time, [a, r, d]) = m.display_columns();
            table.add_row(vec![format!("{:?}", s), time, a, r, d]);
        }

        writeln!(writer, "Performance report")?;
        writeln!(writer, "{}", table)?;

        Ok(())
    }

    pub fn print_per_file_perf_report(&self, writer: &mut impl Write) -> std::io::Result<()> {
        for file in &self.files {
            let mut table = Table::new();
            table.load_preset(TABLE_PRESET);
            table.set_header(vec!["Stage", "Time", "alloc", "realloc", "dealloc"]);

            for (s, m) in &file.entries {
                let (time, [a, r, d]) = m.display_columns();

                table.add_row(vec![format!("{:?}", s), time, a, r, d]);
            }

            writeln!(writer, "File: {}", file.name)?;

            writeln!(writer, "{}", table)?;
            writeln!(writer)?;
        }
        Ok(())
    }
}

// const TABLE_PRESET: &str = "  - ----     - --  ";
const TABLE_PRESET: &str = "  --====     ------";
// const TABLE_PRESET: &str = comfy_table::presets::ASCII_HORIZONTAL_BORDERS_ONLY;
