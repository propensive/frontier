package experiment

import scala.quoted.*

object Foo:
  given foo(using Bar): Foo = Foo()

object Bar:
  given bar(using Baz): Bar = Bar()

case class Foo()
case class Bar()
case class Baz()

given foo(using Baz): Foo = Foo()

inline def search: Unit = ${doSearch}

def doSearch(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  import dotty.tools.dotc.ast.Trees.SearchFailureIdent


  def missing(tree: Tree): List[SearchFailureIdent[?]] = 
  
  def searchFor(repr: TypeRepr): Unit =
    Implicits.search(repr) match
      case a: ImplicitSearchSuccess => println(a.getClass)
      case b: Tree => b match
        case Apply(id, List(x: SearchFailureIdent[?])) => x.tpe match
          case x: dotty.tools.dotc.typer.Implicits.NoMatchingImplicits => x.expectedType match
            case x: TypeRef => x.asType match
              case '[from] =>
                repr.asType match
                  case '[to] =>
                    TypeRepr.of[from ?=> to]
          case y => println(y)

        case other => println("Not matched: "+other)

  searchFor(TypeRepr.of[Foo])

  '{()}
