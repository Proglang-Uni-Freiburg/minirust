#[cfg(test)]
mod tests {
    use test_macros::{succeeds, typing_fails};

    #[test]
    fn main_exists() {
        succeeds! {
            fn main() {
               ()
            }
        }
    }

    #[test]
    fn main_not_exists() {
        typing_fails! {
            struct main !N

            fn not_main() {
               ()
            }
        }
    }

    #[test]
    fn main_no_args() {
        typing_fails! {
            fn main(arg: ()) {
               ()
            }
        }
    }

    #[test]
    fn main_no_return() {
        typing_fails! {
            fn main() -> Int {
               ()
            }
        }
    }
}
