package demo

import java.util.UUID

class RestApi {


  var state = Map[String,Int]()

//  def POST(item : Int) : String = {
//    val key = UUID.randomUUID().toString
//    state.put(key,item)
//    key
//  }

  def PUT(id : String, item : Int) : String = {
    state = state + (id ->item)
    id
  }

  def GET(id : String) : Int  = {
    state.get(id).get
  }

  def DELETE(id : String) : Boolean = {
    val exists = state.contains(id)
    state = state - id
    return exists
  }

}
