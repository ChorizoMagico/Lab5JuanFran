import Benchmark._
import kmedianas2D._

println("=== PRUEBA INICIAL SIMPLIFICADA ===")

// Prueba 1: Solo generar puntos y clasificar
println("1. Generando puntos...")
val puntos = generarPuntos(3, 50).toSeq
println(s"Puntos generados: ${puntos.length}")

println("2. Inicializando medianas...")
val medianas = inicializarMedianas(3, puntos)
println(s"Medianas iniciales: ${medianas.length}")

println("3. Probando clasificación secuencial...")
val clasifSeq = clasificarSeq(puntos, medianas)
println(s"Clasificación exitosa - clusters: ${clasifSeq.size}")

println("4. Probando clasificación paralela...")
val clasifPar = clasificarPar(1000)(puntos, medianas)
println(s"Clasificación paralela exitosa - clusters: ${clasifPar.size}")

println("5. Probando actualización...")
val actualSeq = actualizarSeq(clasifSeq, medianas)
val actualPar = actualizarPar(clasifPar, medianas)
println(s"Actualización exitosa")

println("6. Probando convergencia...")
val converge = hayConvergenciaSeq(0.1, medianas, actualSeq)
println(s"Convergencia: $converge")

// Solo si lo anterior funciona, probar el algoritmo completo
println("7. Probando kMedianasSeq (con límite de seguridad)...")
try {
  val resultadoSeq = kMedianasSeq(puntos, medianas, 0.01)
  println(s"kMedianasSeq exitoso - medianas finales: ${resultadoSeq.length}")

  println("8. Probando kMedianasPar...")
  val resultadoPar = kMedianasPar(puntos, medianas, 0.01)
  println(s"kMedianasPar exitoso - medianas finales: ${resultadoPar.length}")

  println("9. Midiendo tiempos...")
  val (tiempoSeq, tiempoPar, aceleracion) = tiemposKmedianas(puntos, 3, 0.01)
  println(s"Tiempos - Seq: $tiempoSeq, Par: $tiempoPar, Aceleración: $aceleracion")

} catch {
  case e: Exception =>
    println(s"Error en kMedianas: ${e.getMessage}")
    e.printStackTrace()
}

println("=== PRUEBA COMPLETADA ===")

println("=== PRUEBAS CON MÁS DATOS ===")

// Prueba con 1000 puntos
val puntos1000 = generarPuntos(5, 1000).toSeq
val (tiempoSeq1000, tiempoPar1000, acel1000) = tiemposKmedianas(puntos1000, 5, 0.01)
println(s"1000 puntos - Seq: $tiempoSeq1000, Par: $tiempoPar1000, Acel: $acel1000")

// Prueba con 5000 puntos
val puntos5000 = generarPuntos(8, 5000).toSeq
val (tiempoSeq5000, tiempoPar5000, acel5000) = tiemposKmedianas(puntos5000, 8, 0.01)
println(s"5000 puntos - Seq: $tiempoSeq5000, Par: $tiempoPar5000, Acel: $acel5000")


println("=== PRUEBA CON 10000 PUNTOS ===")
val puntos10000 = generarPuntos(10, 10000).toSeq
val (tiempoSeq10000, tiempoPar10000, acel10000) = tiemposKmedianas(puntos10000, 10, 0.01)
println(s"10000 puntos - Secuencial: $tiempoSeq10000, Paralelo: $tiempoPar10000, Aceleración: $acel10000")

// Generar visualización
probarKmedianas(puntos1000, 5, 0.01)
probarKmedianas(puntos5000, 5, 0.01)
probarKmedianas(puntos10000, 5, 0.01)
println("Archivos HTML generados!")