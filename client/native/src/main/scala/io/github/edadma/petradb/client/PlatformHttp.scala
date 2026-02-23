package io.github.edadma.petradb.client

import scala.concurrent.{Future, ExecutionContext}

def platformPost(url: String, body: String, headers: Map[String, String])(implicit ec: ExecutionContext): Future[HttpResponse] =
  Future.failed(new NotImplementedError("HTTP client not implemented for Scala Native"))

def platformDelete(url: String, headers: Map[String, String])(implicit ec: ExecutionContext): Future[HttpResponse] =
  Future.failed(new NotImplementedError("HTTP client not implemented for Scala Native"))
