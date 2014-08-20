
let run f v = ignore (Thread.create f v)
