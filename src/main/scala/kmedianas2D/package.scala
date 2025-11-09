import scala.annotation.tailrec
import scala.util.Random

package object kmedianas2D {

  import scala.annotation.tailrec
  import scala.collection.{Map, Seq}
  import scala.collection.parallel.CollectionConverters._
  import scala.util.Random
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import common._


  def umbral(tamano: Int): Int = {
    // Puedes ajustar este valor según lo que necesites
    // Un valor común es usar un porcentaje del tamaño total
    math.max(1000, tamano / 10)
  }
  
  /*
  Se define la clase punto. Esta tiene sus coordenadas cartesianas x, y como reales.

  Tiene una función para calcular el cuadrado de un real, y otra función que dado otro punto devuelve
  el cuadrado de la distancia entre las coordenadas de ambos puntos.

  También tiene un método para volver entero un real, y un método que, cuando se declara un punto, lo
  devuelve en el output como dos enteros en lugar de dos reales
   */
  class Punto (val x: Double, val y: Double) {
    private def cuadrado(v: Double): Double = v * v

    def distanciaAlCuadrado(that: Punto): Double =
      cuadrado(that.x -x) + cuadrado(that.y -y )

    private def round(v: Double): Double = (v * 100).toInt / 100.0

    override def toString = s"(round(x).{round(y)})"
  }

  /*
    Función que dado una serie de puntas llamada medianas y un punto dado, devuelve la mediana más cercana a ese punto.

    Primero, se asegura de que las medianas sean no vacías, luego a cada punto en las medianas
    le calcula su distancia con respecto al punto original, y ordena de menor a mayor. Selecciona el primero.
     */
  def hallarPuntoMasCercano(p: Punto, medianas: Seq[Punto]): Punto = {
    assert(medianas.nonEmpty)
    medianas.map(pto => (pto, p.distanciaAlCuadrado(pto))).sortWith((a, b) => (a._2 < b._2)).head._1
  }

  /*
  Aplica la función anterior a un conjunto de puntos, y los agrupa en un diccionario (o colección Map),
  el cual es un conjunto de pares claves-valor, y solo se puede acceder a un valor a través de su clave.

  Así, los valores son un conjunto de puntos (clústers) y la clave es la mediana más cercana a ellos
   */
  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    require(medianas.nonEmpty)

    puntos.groupBy(p => hallarPuntoMasCercano(p, medianas))
  }


  /*
  Versión paralela de tareas de la función anterior. Esta recibe un umbral, que es el número de puntos a partir
  del cuál se hace de forma paralela. Si el número de puntos es menor al umbral, simplemente
  se llama a la función secuencial. Si no, se dividen los puntos a la mitad, y se crea un hilo para cada mitad.
  En cada hilo se llama a la función paralela nuevamente, la cual vuelve a evaluar el umbral, hasta que
  llegue al punto en que se llame a clasificarSeq.

  Hay una función auxiliar combinarMapas que combina todas las mitades en la colección Map (la de clave-valor)
  correspondiente
   */
  def clasificarPar(umb: Int)(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    require(medianas.nonEmpty)

    def combinarMapas(m1: Map[Punto, Seq[Punto]], m2: Map[Punto, Seq[Punto]]): Map[Punto, Seq[Punto]] = {
      (m1.keySet ++ m2.keySet).map { k =>
        k -> (m1.getOrElse(k, Seq()) ++ m2.getOrElse(k, Seq()))
      }.toMap
    }


    if(puntos.length <= umb) {
      clasificarSeq(puntos, medianas)
    } else {
      val(izq, der) = puntos.splitAt(puntos.length / 2)

      val tareaIzq = task { clasificarPar(umb)(izq, medianas)}
      val tareaDer = task { clasificarPar(umb)(der, medianas)}

      val resIzq = tareaIzq.join
      val resDer = tareaDer.join

      combinarMapas(resIzq, resDer)
    }

  }

/*
Dado una secuencia de puntos, halla el promedio de ellos (el promedio de sus componentes en x
  y el promedio de sus componentes en y). Esa es la nueva mediana. Si no hay puntos, la nueva
  mediana es la vieja (o anterior)
 */
  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {

      new Punto( puntos.map( p=> p.x).sum / puntos.length, puntos.map(p=> p.y).sum / puntos.length)
    }
  }

/*
Función idéntica a la anterior pero usa paralelismo de datos al trasformar la secuencia de puntos
en una secuencia paralela de puntos (usando puntos.par)
 */
  def calculePromedioPar (medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
    if(puntos.isEmpty) medianaVieja
    else {
      val puntosPar = puntos.par
      new Punto(puntosPar.map(p=>p.x).sum /puntosPar.size, puntosPar.map(p=>p.y).sum/puntosPar.size)
    }
  }

/*
Función que para cada mediana en la colección Map (o diccionario) calcula una nueva mediana
usando la función calculePromedioSeq definida anteriormente
 */
  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq [Punto] = {

    for( i <- medianasViejas) yield {
      val puntosAsociados = clasif.getOrElse(i, Seq())
      calculePromedioSeq(i, puntosAsociados)
    }
  }

  /*
  Función que para cada mediana en la colección Map (o diccionario) calcula una nueva mediana
  usando la función calculePromedioPar definida anteriormente
   */
  def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {

    for (i <- medianasViejas) yield {
      val puntosAsociados = clasif.getOrElse(i, Seq())
      calculePromedioPar(i, puntosAsociados)
    }
  }

  /*
  Función que compara la distancia entre los dos conjuntos de medianas, y si todas las diferencias son menores a
  un real dado eta, devuelve true, es decir, que hay convergencia.
  Esto lo realiza haciendo comparaciones (&&) sobre ambos head de ambas secuencias y llamando de nuevo a la
  función con el resultado de la anterior comparación y con el tail de las secuencias
   */
  def hayConvergenciaSeq(eta: Double, medianasViejas: Seq[Punto],
                         medianasNuevas: Seq[Punto]): Boolean ={

    @tailrec
    def auxConvergenciaIterativa(acum: Boolean, m1: Seq[Punto], m2: Seq[Punto]): Boolean = {

      if (m1.isEmpty || m2.isEmpty) acum
      else if (!acum) false
      else{

        val converge = m1.head.distanciaAlCuadrado(m2.head) < eta

        auxConvergenciaIterativa(acum && converge, m1.tail, m2.tail)
      }
    }

    auxConvergenciaIterativa(true, medianasViejas, medianasNuevas)
  }

  /*
  Función con finalidad idéntica a la anterior, pero paralela. Si las secuencias son de tamaño 1 o menor,
  se llama a la versión secuencial. Si no, las divide en dos y llama un hilo para cada mitad, el cual
  vuelve a llamar a la función con la secuencia partida, y así hasta que se llame a la función secuencial.
  Todos los resultados se combinan con &&
   */
  def hayConvergenciaPar (eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {

    require(medianasViejas.length == medianasNuevas.length)

    if(medianasViejas.length <=1) hayConvergenciaSeq(eta, medianasViejas, medianasNuevas)
    else{
      val(izq1, der1) = medianasViejas.splitAt(medianasViejas.length / 2)
      val(izq2, der2) = medianasNuevas.splitAt(medianasNuevas.length / 2)

      val tareaIzq = task {
        hayConvergenciaPar(eta, izq1, izq2)
      }
      val tareaDer = task {
        hayConvergenciaPar(eta, der1, der2)
      }

      val resIzq = tareaIzq.join
      val resDer = tareaDer.join

      resIzq && resDer

    }
  }

  /*
  Implementa el algoritmo completo con las funciones secuencias.

  Funciona de esta manera: Dada una secuencia de puntos, unas medianas dadas y un real eta,
  ordena los puntos con las medianas que les corresponda, luego calcula unas nuevas medianas
  usando el promedio de las viejas, verifica si convergen estas nuevas medianas (si no están
  muy alejadas de las anterior) y si convergen, las devuelve. Si no convergen, vuelve a llamar a la función
  pero ahora con las nuevas medianas
   */
  final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {

    @tailrec
    def auxKMean(puntos: Seq[Punto], medianas: Seq[Punto]): Seq[Punto] = {

      val mapViejo = clasificarSeq(puntos, medianas)
      val medianasNuevas = actualizarSeq(mapViejo, medianas)
      val hayConvergencia = hayConvergenciaSeq(eta, medianas, medianasNuevas)
      if(hayConvergencia) medianasNuevas
      else auxKMean(puntos, medianasNuevas)
    }

    auxKMean(puntos, medianas)
  }

  /*
  Función idéntica a la anterior pero con las funciones paralelas
   */
  final def kMedianasPar(puntos: Seq[Punto] , medianas: Seq[Punto] , eta: Double): Seq[Punto] = {

    @tailrec
    def auxKMean(puntos: Seq[Punto], medianas: Seq[Punto]): Seq[Punto] = {

      val mapViejo = clasificarPar(5)(puntos, medianas)
      val medianasNuevas = actualizarPar(mapViejo, medianas)
      val hayConvergencia = hayConvergenciaPar(eta, medianas, medianasNuevas)
      if (hayConvergencia) medianasNuevas
      else auxKMean(puntos, medianasNuevas)
    }

    auxKMean(puntos, medianas)
  }

  def generarPuntos(k: Int, num: Int): Seq[Punto] = {
    val randx = new Random
    val randy = new Random

    (0 until num).map { i =>
      val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
      val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
      new Punto(x, y)
    }
  }

  def  inicializarMedianas(k: Int , puntos: Seq[Punto]): Seq[Punto] = {
    val rand = new Random
    (0 until k).map(_ => puntos(rand.nextInt (puntos.length)))
  }

  }




