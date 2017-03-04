package chetak.server.http

import chetak.server.http.HttpRequestMatcher.{RequestHandler, RequestMatcher}


/**
  * This is class which matches incoming requests to its respective handlers
  * It also has some utility methods to retrieve query and path parameters and perform some arbitrary validation
  *
  * Created by tushark on 14/1/17.
  */
object HttpRequestMatcher {

  type RequestMatcher = (HttpRequest => Boolean)

  type RequestHandler = (HttpRequest => HttpResponse)

  val notimplementedHTML = "<html>\r\n" +
    "<body>\r\n" +
    "<h1>Not Implemented!</h1>\r\n" +
    "</body>\r\n" +
    "</html> \r\n"

  val QUERY_PARAMETERS_EXPECTED = Http.build(HttpStatusCodes.BadRequest, StringBody("Query Parameters expected"))


  val DefaultHandler = { req : HttpRequest =>
    Http.build(HttpStatusCodes.NotImplemented, StringBody(notimplementedHTML), req)
  }


  def path(path: String, children: List[HttpRequestMatch] = List(), handler: RequestHandler = DefaultHandler): HttpRequestMatch =
    HttpRequestMatch(s"path-$path" ,{ req =>
      req.requestLine.uri.startsWith(path)
    }, children, handler)

  def get(children: List[HttpRequestMatch] = List())(handler: RequestHandler = DefaultHandler): HttpRequestMatch =
    HttpRequestMatch("get", { req =>
      req.requestLine.method.equals(HttpMethod.GET)
    }, children, handler)

  def put(children: List[HttpRequestMatch] = List())(handler: RequestHandler = DefaultHandler): HttpRequestMatch =
    HttpRequestMatch("put",{ req =>
      req.requestLine.method.equals(HttpMethod.PUT)
    }, children, handler)

  def post(children: List[HttpRequestMatch] = List())(handler: RequestHandler = DefaultHandler): HttpRequestMatch =
    HttpRequestMatch("post",{ req =>
      req.requestLine.method.equals(HttpMethod.POST)
    }, children, handler)

  def post(handler: RequestHandler): HttpRequestMatch = post(List())(handler)

  def get(handler: RequestHandler): HttpRequestMatch = get(List())(handler)

  def debug(str : String) = {
    //println(str)
  }

  /**
    * This method matches incoming request to matching request handler from the list of HttpRequestMatch provided
    * @param list
    * @param req
    * @return
    */
  def matchRequest(list: List[HttpRequestMatch], req: HttpRequest) : HttpResponse = {
    list match {
      case head :: tail =>
        debug("Calling match " + head.name)
        if(head._match(req)) {
          debug("Match found children " + head.children)
          if(!head.children.isEmpty) {
            debug("Calling children to match")
            matchRequest(head.children, req)
          }
          else {
            debug("Since no children calling its handler")
            try {
              val resp = head.handler(req)
              resp
            } catch {
              case ex: Throwable =>
                Http.error(ex)
            }
          }
        } else {
          debug("No match continuing with rest of the tail")
          matchRequest(tail, req)
        }
      case Nil => {
        debug("Calling default handler")
        DefaultHandler(req)
      }
    }
  }

  type Validator = (HttpRequest => (Boolean, Option[HttpResponse]))

  def validate(req: HttpRequest, v: Validator)(f: RequestHandler) : HttpResponse = {
    val validationResult = v(req)
    if(validationResult._1) {
      f(req)
    } else validationResult._2.get
  }

  def checkValidParams(req: HttpRequest, validParams: List[String])(f: RequestHandler) : HttpResponse = {
    if (!req.requestLine.queryParams.isDefined) {
      QUERY_PARAMETERS_EXPECTED
    } else {
      val invalidParams = req.requestLine.queryParams.get.keys.foldLeft(List[String]())({ (list, key) =>
        if (!validParams.contains(key))
          key :: list
        else list
      })

      if (invalidParams.size > 0) {
        Http.build(HttpStatusCodes.BadRequest, StringBody(s"Wrong Parameters : ${invalidParams.mkString("(",",",")")}"), req)
      } else f(req)
    }
  }

  def checkMandatoryParams(req: HttpRequest, mandatoryParams: List[String])(f: RequestHandler) : HttpResponse= {
    val list = mandatoryParams.foldLeft(List[String]())({ (list, key) =>
      if (!req.requestLine.queryParams.get.contains(key))
        key :: list
      else list
    })

    if(list.size > 0) {
      Http.build(HttpStatusCodes.BadRequest, StringBody(s"MandatoryParams Parameters Missing: ${list.mkString("(",",",")")}"), req)
    } else {
      f(req)
    }
  }

  def queryParamDouble(req: HttpRequest, str: String) : Double = {
    req.requestLine.queryParams.get.get(str).get.toDouble
  }

  def queryParamInt(req: HttpRequest, str: String) : Int = {
    req.requestLine.queryParams.get.get(str).get.toDouble.toInt
  }

  def queryParamDoubleOpt(req: HttpRequest, str: String, default: Double) : Double = {
    req.requestLine.queryParams.get.get(str) match {
      case Some(x) => x.toDouble
      case None => default
    }
  }

  def queryParamIntOpt(req: HttpRequest, str: String, default: Int) : Int = {
    req.requestLine.queryParams.get.get(str) match {
      case Some(x) => x.toInt
      case None => default
    }
  }

  import scala.reflect.runtime.universe._

  def pathParam[T : TypeTag](req: HttpRequest, pattern: String, default: T) : Option[T] = {
    pattern.r.findFirstMatchIn(req.requestLine.uri) match {
      case Some(x) =>
        val group = x.group(1)
        typeOf[T] match {
          case t if t =:= typeOf[String] =>
            Some(group).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Int] =>
            Some(group.toInt).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Double] =>
            Some(group.toDouble).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Boolean] =>
            Some(group.toBoolean).asInstanceOf[Option[T]]
        }
      case None => Some(default)
    }
  }

  def pathParam[T : TypeTag](req: HttpRequest, pattern: String) : Option[T] = {
     pattern.r.findFirstMatchIn(req.requestLine.uri).map { x =>
      val group = x.group(1)
      typeOf[T] match {
        case t if t =:= typeOf[String] =>
          Some(x).asInstanceOf[T]
        case t if t =:= typeOf[Int] =>
          Some(group.toInt).asInstanceOf[T]
        case t if t =:= typeOf[Double] =>
          Some(group.toDouble).asInstanceOf[T]
        case t if t =:= typeOf[Boolean] =>
          Some(group.toBoolean).asInstanceOf[T]
      }
    }
  }

  def queryParam[T : TypeTag](req: HttpRequest, str: String, default: T) : Option[T] = {
    req.requestLine.queryParams.get.get(str) match {
      case Some(x) =>
        typeOf[T] match {
          case t if t =:= typeOf[String] =>
            Some(x).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Int] =>
            Some(x.toInt).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Double] =>
            Some(x.toDouble).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Float] =>
            Some(x.toFloat).asInstanceOf[Option[T]]
          case t if t =:= typeOf[Boolean] =>
            Some(x.toBoolean).asInstanceOf[Option[T]]
        }
      case None => Some(default)
    }
  }


  def pathParamInt(req: HttpRequest, pattern: String) : Option[Int] = {
    pattern.r.findFirstMatchIn(req.requestLine.uri).map { x => x.group(1).toInt }
  }


}

case class HttpRequestMatch(name: String, _match: RequestMatcher, children: List[HttpRequestMatch], handler: RequestHandler)
