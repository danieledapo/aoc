use structopt::StructOpt;

#[derive(StructOpt, Debug)]
struct App {
    #[structopt(subcommand)]
    animation: Animation,
}

#[derive(StructOpt, Debug)]
enum Animation {
    /// Day 15 animation. Goblins vs Elves
    #[structopt(name = "day15")]
    Day15,

    /// Day 18 animation. Lumberyard automata.
    #[structopt(name = "day18")]
    Day18,
}

fn main() {
    let app = App::from_args();

    match app.animation {
        Animation::Day15 => animation_day15(),
        Animation::Day18 => animation_day18(),
    };
}

fn animation_day15() {
    use aoc2018::day15::{Game, GamePrinter, GameState};

    let input = include_str!("../input/day15.txt");
    let mut game = Game::from_str(4, input).unwrap();

    loop {
        println!(
            "{}{}{}",
            termion::clear::All,
            termion::cursor::Goto(1, 1),
            GamePrinter {
                game: &game,
                with_colors: true
            }
        );

        if game.play_next_round(false) == GameState::End {
            println!(
                "{}{}{}",
                termion::clear::All,
                termion::cursor::Goto(1, 1),
                GamePrinter {
                    game: &game,
                    with_colors: true
                }
            );

            return;
        }

        std::thread::sleep(std::time::Duration::from_millis(200));
    }
}

fn animation_day18() {
    use aoc2018::day18::{Area, AreaPrinter};

    let input = include_str!("../input/day18.txt");
    let mut state = input.parse::<Area>().unwrap();

    loop {
        println!(
            "{}{}{}",
            termion::clear::All,
            termion::cursor::Goto(1, 1),
            AreaPrinter {
                area: &state,
                with_colors: true
            }
        );

        state = state.evolve();

        std::thread::sleep(std::time::Duration::from_millis(50));
    }
}
