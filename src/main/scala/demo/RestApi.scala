package demo

import java.util.UUID

object RestApi{

  var state = Map[String,Int]()

}

class RestApi {



//  def POST(item : Int) : String = {
//    val key = UUID.randomUUID().toString
//    state.put(key,item)
//    key
//  }

  def PUT(id : String, item : Int) : String = {
    RestApi.state = RestApi.state + (id ->item)
    id
  }

  def GET(id : String) : Int  = {
    RestApi.state.get(id).get
  }

  def DELETE(id : String) : Boolean = {
    val exists = RestApi.state.contains(id)
    RestApi.state = RestApi.state - id
    return exists
  }

}
