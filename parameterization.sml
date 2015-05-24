signature ORDERED =
  sig
    type t
    val lt : t * t -> bool
    val eq : t * t -> bool
  end

signature DICTIONARY =
  sig
    structure Key : ORDERED
    type 'a dictionary
    val empty : 'a dictionary
    val insert : 'a dictionary * Key.t * 'a -> 'a dictionary
    val lookup : 'a dictionary * Key.t -> 'a option
  end

functor DictionaryFunctor
  (structure K : ORDERED) :>
    DICTIONARY where type Key.t = K.t =
  struct
    structure Key : ORDERED = K
    datatype 'a dictionary =
      Empty |
      Node of 'a dictionary * Key.t * 'a * 'a dictionary
    val empty = Empty
    fun insert (None, k, v) = 
      Node (Empty, k, v, Empty)
    fun lookup (Empty, _) = NONE
      | lookup (Node (dl, l, v, dr), k) = 
        if Key.lt (k, l) then
          lookup (dl, k)
        else if Key.lt (l, k) then
          lookup (dr, k)
         else
           SOME v
  end

structure LessInt : ORDERED =
  struct
    type t = int
    val eq = (op =)
    val lt = (op <)
  end

structure DivInt : ORDERED =
  struct
    type t = int
    fun lt (m, n) = (n mod m = 0)
    fun eq (m, n) = lt (m, n) andalso lt (n, m)
  end

structure LexString : ORDERED =
  struct
    type t = string
    val eq : string * string -> bool = (op =)
    val lt : string * string -> bool = (op <)
  end

structure LtIntDictionary = DictionaryFunctor (structure K = LessInt)
structure LexStringDictionary = DictionaryFunctor (structure K = LexString)
structure DivIntDictionary = DictionaryFunctor (structure K = DivInt)

(*signature GEOMETRY =
  sig
    structure Point : POINT
    structure Sphere : SPHERE
    sharing Point = Sphere.Point
        and Point.Vector = Sphere.Vector
        and Sphere.Vector = Sphere.Point.Vector
  end

functor PointFunctor 
  (structure V : VECTOR) : POINT = ...
functor SphereFunctor
  (structure V : VECTOR
   structure P : POINT) : SPHERE =
struct
  structure Vector = V
  structure Point = P
  .
  .
  .
end

functor GeometryFunctor
  (structure P : POINT
   structure S : SPHERE) : GEOMETRY =
struct
  structure Point = P
  structure Sphere = S
end

structure Vector2D : VECTOR = ...
structure Point2D : POINT =
  PointFunctor (structure V = Vector2D)
structure Sphere2D : SPHERE =
  SphereFunctor (structure V = Vector2D and P = Point2D)
structure Geometry2D : GEOMETRY =
  GeometryFunctor (structure P = Point2D and S = Sphere2D)

functor SphereFunctor
  (structure V : VECTOR
   structure P : POINT
   sharing P.Vector = V) : SPHERE =
struct
  structure Vector = V
  structure Point = P
  .
  .
  .
end
functor GeometryFunctor
  (structure P : POINT
   structure S : SPHERE
   sharing P.Vector = S.Vector and P = S.Point) : GEOMETRY =
struct
  structure Point = P
  structure Sphere = S
end

signature EXTENDED_GEOMETRY =
  sig
    structure Sphere : SPHERE
    structure SemiSpace : SEMI_SPACE
    sharing Sphere.Point = SemiSpace.Point
  end

functor EXTENDED_GEOMETRY_FUNCTOR
  (structure Sp : SPHERE
   structure Ss : SEMI_SPACE
   sharing Sphere.Point = SemiSpace.Point) =
struct
  structure Sphere = Sp
  structure SemiSpace = Ss
end

functor SphereFunctor
  (structure P : POINT) : SPHERE =
struct
  structure Vector = P.Vector
  structure Point = P
  .
  .
  .
end
functor SemiSpaceFunctor
  (structure P : POINT) : SEMI_SPACE =
struct
  .
  .
  .
end
functor ExtendedGeometryFunctor
  (structure P : POINT) : GEOMETRY = 
struct
  structure Sphere =
    SphereFunctor (structure P = Point)
  structure SemiSpace = 
    SemiSpaceFunctor (structure P = Point)
end

functor ExtendedGeometryFunctor
  (structure P : POINT
   structure Sp : SPHERE where Point = P
   structure Ss : SEMI_SPACE where Point = P) =
struct
  structure Sphere = Sp
  structure SemiSpace = Ss
end

functor ExtendedGeometryFunctor
  (structure Sp : SPHERE
   structure Ss : SEMI_SPACE where Point = Sp.Point) =
struct
  structure Sphere = Sp
  structure SemiSpace = Ss
end

functor ExtendedGeometryFunctor
  (structure Ss : SEMI_SPACE
   structure Sp : SPHERE where Point = Sp.Point) =
struct
  structure Sphere = Sp
  structure SemiSpace = Ss
end*)
