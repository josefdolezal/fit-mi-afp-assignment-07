# MI-AFP homework #07

Homework to practice Monoid, Functor, Applicative, and Monad

## Task

In this homework you will have two tasks that include use of newly learned typeclasses.

1. *Data.SortedList* = implement all necessary functions to make data type `SortedList` to be an instance of `Semigroup`, `Monoid`, `Functor`, `Applicative`, `Monad`. After applying `(<>)` the resulting list must be sorted. Sadly, `Functor`, `Applicative`, and `Monad` won't allow you to use `Ord` constaint so the result won't be sorted. Implement some sort function if you want to... It is **forbidden** to just delegate to list and use `fromList` and `toList`. 
2. *Calculator* = this task is more *free* (in terms of creativity). You have to write a simple CLI calculator with given specification. Try to run `stack exec calculator` and/or see the specs. There are lots of different errors so you should use `Either` monad to the maximum. Consider also about using `<$>` and `<*>`... The implementation should be as simple as possible by using those (no painful nesting of `case-of`). Tests work just with `calculate` function so it is up to you what other functions will you use. You are also required to implement simple CLI interface (show help or calculate per line).

If it was too easy for you, please read some additional materials on Monads and try to devise some extension of *Calculator* on your own (memory, composition, infix operators, etc.).

Feel free to write additional tests as well as a documentation for your implementation (not graded).

## Notes

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * Follow **DRY** principle, use syntactic sugar when you can, make your code clean and understandable.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
