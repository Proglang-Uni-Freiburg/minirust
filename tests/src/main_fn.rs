#[cfg(test)]
mod tests {
    use macros::{succeeds, typing_fails};

    #[test]
    fn main_exists() {
        succeeds! {
             use tests::util::assert;

             fn main() {
                 assert::assert(true, "never happens")
             }
        }
    }

    #[test]
    fn main_not_exists() {
        typing_fails! {
            struct main

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
