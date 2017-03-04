package chetak

import chetak.server.http.HttpRequestMatcher._
import chetak.server.http.{HttpRequest, HttpRequestMatch, HttpService}

/**
  * Created by tushark on 3/3/17.
  */
object KeyValueService extends HttpService {

  //using java maps as scala collections are known to be performing worse than java maps
  val mapOfMaps = new java.util.concurrent.ConcurrentHashMap[String, java.util.concurrent.ConcurrentHashMap[Any, Any]]()

  def addNamedMap(name: String): java.util.concurrent.ConcurrentHashMap[Any, Any] = {
    if(!mapOfMaps.contains(name)) {
      mapOfMaps.put(name, new java.util.concurrent.ConcurrentHashMap[Any, Any])
    } else mapOfMaps.get(name)
  }

  def deleteNamedMap(name: String): Option[java.util.concurrent.ConcurrentHashMap[Any, Any]] = {
    if(mapOfMaps.contains(name)) {
      Some(mapOfMaps.remove(name))
    } else
      None
  }


  def put(mapName: String, key: Any, value: Any) : Option[Any] = {
    val namedMap = mapOfMaps.get(mapName)
    if(namedMap !=null) {
      Some(namedMap.put(key, value))
    } else None
  }

  def get(mapName: String, key: Any) : Option[Any] = {
    val namedMap = mapOfMaps.get(mapName)
    if(namedMap !=null) {
      val value = namedMap.get(key)
      if(value !=null)
        Some(value)
      else
        None
    } else
      None
  }

  def delete(mapName: String, key: Any) : Option[Any] = {
    val namedMap = mapOfMaps.get(mapName)
    if(namedMap !=null) {
      val value = namedMap.remove(key)
      if(value !=null)
        Some(value)
      else
        None
    } else
      None
  }


  val createNameMap = { req: HttpRequest =>

  }




  /*
    PUT /kv - create new namedmap

    GET /kv/{name}/keys - keySet
    GET /kv/{name}/size - size
    PUT /kv/{name} - new KV
    POST /kv/{name}/{key} - update key
    DELETE /kv/{name}/{key} - delete key
    DELETE /kv/{name} - delete namedMap

   */

  override def routes: List[HttpRequestMatch] = List(path("/kv", List()))

}
