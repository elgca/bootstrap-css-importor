package elgca.webapp.extops

given asOpts[T]: Conversion[T, Option[T]] with
  inline def apply(x: T): Option[T] = Option(x)
