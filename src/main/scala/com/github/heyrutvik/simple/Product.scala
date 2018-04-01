package com.github.heyrutvik.simple

case class Product(expr: Expr, env: Option[Expr.Env])

object Product {

  def apply(expr: Expr) = new Product(expr, None)
  def apply(expr: Expr, env: Expr.Env) = new Product(expr, Option(env))
}