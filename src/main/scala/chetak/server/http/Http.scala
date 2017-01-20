package chetak.server.http

import java.io.{PrintWriter, StringWriter}

import chetak.json._
import chetak.server.http.HttpMethod.HttpMethod
import chetak.server.http.HttpVersion.HttpVersion

import scala.collection.mutable

/**
  *
  *
  * This is simpler implementation of HTTP protocol and its server components and its scope is limited
  * "Where is my driver" assignment. It implements following things
  *
  * 1. Basic parsing of request and builder methods for forming HTTP response messages
  * 2. TCP Port server with scala futures for getting good performance
  * 3. Simple application helpers to extract path parameters, request parameters and simpler router definitions and json serialization
  * 4. It implements only GET and POST methods
  * 5. It does not implement aspects of http protocols : versions, content encodings, media types, caching, url encodings, security etc.
 *  6. It also not implement **KEEP-ALIVE** which is probably most important tuning since it allows reuse of connections between connections
 */
object Http {

  /** Regex to parse HttpRequest Request Line */
  val REQUEST_LINE_PATTERN = " ".r

  /** Regex to parse out QueryString from HttpRequest */
  val QUERY_STRING_PATTERN = "\\?".r

  /** Regex to parse out parameters from query string */
  val PARAM_STRING_PATTERN = "\\&|;".r  //Delimiter is either & or ;

  /** Regex to parse out key/value pairs */
  val KEY_VALUE_PATTERN = "=".r

  /** Regex to parse raw headers and body */
  val RAW_VALUE_PATTERN = "\\r\\n\\r\\n".r//TODO fix a better regexp for this

  /** Regex to parse raw headers from body */
  val HEADERS_BODY_PATTERN = "\\r\\n".r

  /** Regex to parse header name and value */
  val HEADER_VALUE_PATTERN = ": ".r




  def parse(request: String) : HttpRequest = /*Future */{
    val headersAndBody = RAW_VALUE_PATTERN.split(request)
    val headerFields = HEADERS_BODY_PATTERN.split(headersAndBody(0))
    val requestLine: String = headerFields(0)
    val headers = headerFields.drop(1).map { h =>
      val a = HEADER_VALUE_PATTERN.split(h)
      HttpHeader(a(0).toLowerCase(), a(1))
    }

    val requestLineSplit = REQUEST_LINE_PATTERN.split(requestLine)
    val method = HttpMethod.withName(requestLineSplit(0)) //requestLineSplit(0)

    val pathFrags: Array[String] = QUERY_STRING_PATTERN.split(requestLineSplit(1))
    val path = pathFrags(0)
    val version = requestLineSplit(2) match {
      case "HTTP/1.1" => HttpVersion.VERSION_1_1
      case "HTTP/1.0" => HttpVersion.VERSION_1_0
    }
    val queryParamters = parseParams(requestLineSplit(1))
    HttpRequest(RequestLine(method, path, version, queryParamters), headers, Some(new StringBody(headersAndBody(1))))
  }

  //TODO currently Dummy
  def getCurrentDate() = "Mon, 27 Jul 2009 12:28:53 GMT"

  def defaultHeaders(request: HttpRequest) : List[HttpHeader] = {
    List(
      HttpHeader("Date", getCurrentDate()),
      HttpHeader("Server", "WhereIsMyDriver"),
      HttpHeader("Last-Modified", getCurrentDate())
    )
  }

  def defaultHeaders() : List[HttpHeader] = {
    List(
      HttpHeader("Date", getCurrentDate()),
      HttpHeader("Server", "WhereIsMyDriver"),
      HttpHeader("Last-Modified", getCurrentDate())
    )
  }

  def error(ex: Throwable) : HttpResponse = {
    val sw = new StringWriter()
    val stringWriter = new PrintWriter(sw)
    stringWriter.append("Error processing requests ")
    ex.printStackTrace(stringWriter)
    HttpResponse(HttpStatusCodes.InternalServerError, defaultHeaders() , Some(StringBody(sw.toString)))
  }

  def build(status: HttpStatusCode, body : HttpBody, request: HttpRequest) = {
    HttpResponse(status, defaultHeaders(request) , Some(body))
  }

  def build(status: HttpStatusCode, body : HttpBody) = {
    HttpResponse(status, defaultHeaders() , Some(body))
  }

  def build(status: HttpStatusCode, body : HttpBody, headers: List[HttpHeader], request: HttpRequest) = {
    HttpResponse(status, defaultHeaders(request) ::: headers, Some(body))
  }


  def toArrayString(s: Array[String]) = {
    val sb = new StringBuilder
    s.foreach { sa =>
      sb.append(sa.toString).append(" ")
    }
    sb.toString
  }

  def toArrayString(s: Array[HttpHeader]) = {
    val sb = new StringBuilder
    s.foreach { sa =>
      sb.append(sa.toString).append(" ||")
    }
    sb.toString
  }

  def parseParams(queryStr : String): Option[mutable.Map[String, String]] = {
    val str: Array[String] = QUERY_STRING_PATTERN.split(queryStr)
    if(str.length > 1 ) {

      val paramMap = new mutable.HashMap[String, String]()

      val paramArray: Array[String] = PARAM_STRING_PATTERN.split(str(1))
      for (keyValue <- paramArray) {
        val keyValueArray: Array[String] = KEY_VALUE_PATTERN.split(keyValue)
        //We need to check if the parameter has a value associated with it.
        if (keyValueArray.length > 1)
          paramMap.put(keyValueArray(0), keyValueArray(1)) //name, value
      }
      Some(paramMap)
    } else
      None
  }

}


case class HttpRequest(requestLine: RequestLine, headers: Seq[HttpHeader], body: Option[HttpBody])
case class RequestLine(method: HttpMethod, uri: String, version: HttpVersion, queryParams: Option[mutable.Map[String,String]])
case class HttpHeader(name: String, value: String)



object HttpMethod extends Enumeration {
  type HttpMethod = Value
  val  GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE= Value
}

object HttpVersion extends Enumeration {
  type HttpVersion = Value
  val  VERSION_1_0, VERSION_1_1 = Value
}

trait HttpBody {
  def string() : String
}

case class StringBody(str: String) extends HttpBody {
  override def string(): String = str
}

case class JSONBody(json: JObject) extends HttpBody {
  override def string(): String = JSON.json(json)
}

case class JSONArrayBody(json: JArray) extends HttpBody {
  override def string(): String = JSON.json(json)
}

case class HttpStatusCode(code: Int, reasonPhrase: String)

object HttpStatusCodes {
  val OK = HttpStatusCode(200, "OK")
  val BadRequest = HttpStatusCode(400, "Bad Request")
  val Unauthorized = HttpStatusCode(401, "Unauthorized")
  val Forbidden =  HttpStatusCode(403, "Forbidden")
  val NotFound =  HttpStatusCode(404, "Not Found")
  val RequestTimeout = HttpStatusCode(408, "Request Timeout")
  val UnprocessableEntity = HttpStatusCode(422, "UnprocessableEntity")
  val InternalServerError =   HttpStatusCode(500, "Internal Server Error")
  val NotImplemented = HttpStatusCode(501, "Not Implemented")
  val ServiceUnavailable = HttpStatusCode(503, "Service Unavailable")
  val HTTPVersionNotSupported = HttpStatusCode(505, "HTTP Version Not Supported")
}


case class HttpResponse(status : HttpStatusCode, headers: Seq[HttpHeader], body: Option[HttpBody]) {

  override def toString: String = {
    val CRLF = "\r\n"
    val sb = new StringBuilder
    sb.append("HTTP/1.1").append(" ").append(status.code).append(" ").append(status.reasonPhrase).append(CRLF)
    headers.foreach { header =>
      sb.append(header.name).append(": ").append(header.value).append(CRLF)
    }
    sb.append(CRLF).append(CRLF)

    val bodyString = if(body.isDefined)
      body.get.string()
    else CRLF
    sb.append(bodyString)
    println(s"Response before toString <$bodyString>")
    sb.toString()
  }

}
