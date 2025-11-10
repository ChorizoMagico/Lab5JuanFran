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

// Función para imprimir los resultados
def imprimirResultados(tiempoSeq: org.scalameter.Quantity[Double], tiempoPar: org.scalameter.Quantity[Double], acel: Double, puntos: Int, k: Int) = {
  println(s"Resultados para $puntos puntos y $k clusters:")
  println(s"  - Tiempo Secuencial: $tiempoSeq")
  println(s"  - Tiempo Paralelo: $tiempoPar")
  println(s"  - Aceleración: $acel")
  println()
}

// **Prueba 1**: 512 puntos, 4 clusters
val puntos512 = generarPuntos(4, 512).toSeq
val (tiempoSeq512, tiempoPar512, acel512) = tiemposKmedianas(puntos512, 4, 0.01)
imprimirResultados(tiempoSeq512, tiempoPar512, acel512, 512, 4)

// **Prueba 2**: 1024 puntos, 8 clusters
val puntos1024 = generarPuntos(8, 1024).toSeq
val (tiempoSeq1024, tiempoPar1024, acel1024) = tiemposKmedianas(puntos1024, 8, 0.01)
imprimirResultados(tiempoSeq1024, tiempoPar1024, acel1024, 1024, 8)

// **Prueba 3**: 2048 puntos, 16 clusters
val puntos2048 = generarPuntos(16, 2048).toSeq
val (tiempoSeq2048, tiempoPar2048, acel2048) = tiemposKmedianas(puntos2048, 16, 0.01)
imprimirResultados(tiempoSeq2048, tiempoPar2048, acel2048, 2048, 16)

// **Prueba 4**: 4096 puntos, 32 clusters
val puntos4096 = generarPuntos(32, 4096).toSeq
val (tiempoSeq4096, tiempoPar4096, acel4096) = tiemposKmedianas(puntos4096, 32, 0.01)
imprimirResultados(tiempoSeq4096, tiempoPar4096, acel4096, 4096, 32)

// **Prueba 5**: 8192 puntos, 64 clusters
val puntos8192 = generarPuntos(64, 8192).toSeq
val (tiempoSeq8192, tiempoPar8192, acel8192) = tiemposKmedianas(puntos8192, 64, 0.01)
imprimirResultados(tiempoSeq8192, tiempoPar8192, acel8192, 8192, 64)
// Generar visualización
probarKmedianas(puntos512, 4, 0.01)
probarKmedianas(puntos1024,8, 0.01)
probarKmedianas(puntos2048,16, 0.01)
probarKmedianas(puntos4096,32, 0.01)
probarKmedianas(puntos8192,64, 0.01)

println("Archivos HTML generados!")