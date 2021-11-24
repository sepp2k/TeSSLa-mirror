object Main {

    def main(args: Array[String]) = {
      import TesslaMonitor._
      
      out_z = (value, ts, name, error) => if (error == null) println(s"$ts: $name = $value") else println(s"$ts: $name caught an error")  
        
      set_var_x(3, 0)
      set_var_x(5, 0)
      set_var_y(1, 10)
      set_var_x(4, 12)
      set_var_y(1, 20)
      set_var_x(4, 23)
      flush()
      set_var_y(4, 23)
      
      flush()
    }

}
