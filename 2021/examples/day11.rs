use std::io::{self, stdout, Write};

use termion::{clear, color, cursor};

fn main() -> io::Result<()> {
    let mut octo = aoc2021::day11::parse(include_str!("../input/day11.txt"));

    let out = stdout();
    let mut out = out.lock();

    write!(out, "{}{}", cursor::Save, cursor::Hide)?;

    loop {
        write!(
            out,
            "{}{}{}",
            color::Bg(color::Reset),
            clear::All,
            cursor::Goto(1, 1)
        )?;

        let mut prev_row = 0;
        for (r, _, e) in octo.iter_mut() {
            if r != prev_row {
                prev_row = r;
                writeln!(out, "{}", color::Bg(color::Reset))?;
            }

            write!(
                out,
                "{} ",
                color::Bg(color::Rgb(100 + (*e as f64 / 9.0 * 100.0) as u8, 30, 30))
            )?;
        }

        std::thread::sleep(std::time::Duration::from_millis(100));

        octo.evolve();
    }
}
