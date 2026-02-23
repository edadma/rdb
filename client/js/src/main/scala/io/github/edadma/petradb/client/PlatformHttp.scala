package io.github.edadma.petradb.client

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import scala.scalajs.js.JSConverters.*
import scala.concurrent.{Future, ExecutionContext}

def platformPost(url: String, body: String, headers: Map[String, String])(implicit ec: ExecutionContext): Future[HttpResponse] =
  val allHeaders = (Map("Content-Type" -> "application/json") ++ headers).toJSDictionary
  val init       = js.Dynamic.literal(method = "POST", headers = allHeaders, body = body)
  g.fetch(url, init).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { resp =>
    val status = resp.status.asInstanceOf[Double].toInt
    resp.text().asInstanceOf[js.Promise[String]].toFuture.map(HttpResponse(status, _))
  }

def platformDelete(url: String, headers: Map[String, String])(implicit ec: ExecutionContext): Future[HttpResponse] =
  val init = js.Dynamic.literal(method = "DELETE", headers = headers.toJSDictionary)
  g.fetch(url, init).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { resp =>
    val status = resp.status.asInstanceOf[Double].toInt
    resp.text().asInstanceOf[js.Promise[String]].toFuture.map(HttpResponse(status, _))
  }
