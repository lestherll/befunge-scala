package befunge


trait Direction
case object Up extends Direction
case object Down extends Direction
case object Right extends Direction
case object Left extends Direction


var dir: Direction = Up

// @main def test() = 
//     println(dir)
//     dir = Down
//     println(dir)