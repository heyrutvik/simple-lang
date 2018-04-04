package com.github.heyrutvik.simple

import java.io.PrintWriter
import ConstructImplicits._
import ConstructSyntax._
import fastparse.core.Parsed.{Failure, Success}

import scala.io.Source

object Main extends App {

  if (!(args.length > 1)) {
    println(
      s"""
         |<commnad> -interpret <program.simple>
         |<commnad> -compile <program.simple> -o <program.js>
         |<commnad> -ast <program.simple>
       """.stripMargin)
    sys.exit(1)
  }

  def extension(file: String) = {
    if (!file.endsWith(".simple")) {
      println(
        s"""
           |file name must end with ".simple"
         """.stripMargin)
      sys.exit(1)
      false
    } else true
  }

  def fileContents(file: String) = {
    val source = Source.fromFile(file)
    val code = source.getLines.mkString("\n")
    source.close()
    code
  }

  def parse(content: String): Either[String, Expr] = {
    Parser.statement.parse(content) match {
      case Success(expr, _) => Right(expr)
      case Failure(_, _, extra) => Left(extra.traced.expected)
    }
  }

  val output = args.toList match {
    case "-interpret" :: from :: Nil if extension(from) =>
      parse(fileContents(from)) match {
        case Right(expr) => Machine.envPrint(Machine.interpreter(expr, Map.empty).env)
        case Left(s) => s
      }
    case "-compile" :: from :: "-o" :: to :: Nil if extension(from) =>
      parse(fileContents(from)) match {
        case Right(expr) =>
          val a = new PrintWriter(to) { write(Machine.compileToJs(expr, Map.empty)); close }
          s"compiled, $to file contains javascript code."
        case Left(s) => s
      }
    case "-ast" :: from :: Nil if extension(from) =>
      parse(fileContents(from)) match {
        case Right(expr) => expr.string
        case Left(s) => s
      }
    case _ => sys.exit(1)
  }

  println(output)
}
