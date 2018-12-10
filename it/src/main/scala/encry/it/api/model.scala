package encry.it.api

case class UnexpectedStatusCodeException(requestUrl: String, statusCode: Int, responseBody: String)
  extends Exception(s"Request: $requestUrl; Unexpected status code ($statusCode): $responseBody")
