class MyObject[A](y: Long) {

    def func(x: Long) : Int = (x + y).toInt
    
    def func2(x: A) : A = x

}

object MyObject {

//Wrapper function since o.func etc. cannot be directly accessed through the native interface
def func(o: MyObject[_], x: Long) = o.func(x)
def func2[A](o: MyObject[A], x: A) = o.func2(x)

def javaIntToTesslaInt(i: Int): Long = i.toLong

}
