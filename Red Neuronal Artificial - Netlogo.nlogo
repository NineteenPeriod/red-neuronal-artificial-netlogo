links-own [ weight ]
extensions [matrix array rnd]
globals[n crom cromToMute res resDos indexes cromLista matriz matriz2 matrizOrdenada pob valores aux i j k m suma lista valoresR columnaH cromM2 cromM1 puc cont egmin egmax arraysize aproximacion1 aproximacion2 aproximacion3
  nodo-entrada-1   ;; VALORES DE ENTRADA Y SALIDA
  nodo-entrada-2   ;; LOS PONEMOS EN GLOBALS PARA PODER ACCEDER A SUS VALORES DESDE CUALQUIER PARTE
  nodo-salida-1    ;; DIRECTAMENTE
  sinapsis         ;; VARIABLE GLOBAL DEL TAMAÑO DE LA SINAPSIS
  pob-list         ;; LISTA DE LA POBLACION
  index            ;;
  matrizdepoblacion;; MATRIZ DE POBLACION
  sinapsismasuno   ;; VARIABLE GLOBAL DE LA SINAPSIS
  generaciones
  error-c-medio
  error-generacion error-generacion-old matrizNew mejorValor matrizOld
]

breed [ nodos-entrada nodo-entrada ]
breed [ nodos-salida nodo-salida ]
breed [ nodos-ocultos nodo-oculto ]


turtles-own [
  activacion     ;; DETERMINA LA SALIDA DE LOS NODOS
                 ;; ACTIVACION SIGNIFICA EL ESTIMULO QUE RECIBEN LOS NODOS DE ENTRADA
  err            ;; ERROR USADO POR EL BACK-PROPAGATION PARA PROPAGAR EL ERROR
]

;;;                               ;;;
;;; PROCEDIMIENTOS DE PREPARACION ;;;
;;;                               ;;;

to setup                                                    ;; ESTE PROCEDIMIENTO REALIZA EL SET-UP DE LA RED
  clear-all                                                 ;; LIMPIA TODO
  reset-ticks
  ask patches [ set pcolor black ]
  set-default-shape nodos-entrada "input-node"              ;; ESTABLECE LAS FORMAS DE LOS NODOS
  set-default-shape nodos-salida "output-node"              ;;
  set-default-shape nodos-ocultos "hidden-node"             ;;
  set-default-shape links "default"               ;;
  setup-nodes                                               ;; SETUP DE LOS NODOS
  set sinapsis count turtles with [breed = nodos-ocultos]   ;; CUENTA LA CANTIDAD DE NODOS OCULTOS Y LO ASIGNA A SINAPSIS
  set sinapsis 2 * sinapsis + 1 * sinapsis                  ;; CALCULA LA CANTIDAD DE ENLACES CON LA FORMULA 2*n+1*cantidad-de-nodos
  rna                                                       ;; PROCESO DE GENERACION DE LA RED NEURONAL
end

to rna
  set crom calcular_crom a b p
  set error-generacion 0
  output-print ""
  output-print "Tamaño de la poblacion:"
  output-print tam_poblacion
  output-print ""
  output-print "Tamaño del cromosoma:"
  output-print crom
  output-print ""
  output-print "Numero de conexiones:"
  output-print sinapsis

  set sinapsismasuno sinapsis + 1
  set matrizdepoblacion matrix:from-row-list n-values tam_poblacion [n-values sinapsismasuno [ -> random 2]]

  set index 0
  while[index < tam_poblacion]
  [
    ;output-print ""
    ;output-print "Persona numero:"
    ;output-print index
    ;output-print "Lista de pesos:"
    set matriz matrix:from-row-list n-values sinapsis [n-values crom [ -> random 2]] ;;CREA EL UN INDIVIDUOS
    ;output-print matrix:to-row-list matriz
    set valores evalua matriz sinapsis crom
    set valoresR aproximaciones valores sinapsis

    set columnaH array:to-list valoresR
    set valores array:to-list valores

    ;output-print ""
    ;output-print "Pesos en decimal"
    ;output-print valores

    ;output-print ""
    ;output-print "Pesos dentro del rango -1, 1"
    ;output-print columnaH

    clear-links
    setup-enlaces-1
    propagar
    ask nodo-salida-1 [
      set res err
    ]
    set columnaH insert-item sinapsis columnaH res

    matrix:set-row matrizdepoblacion index columnah


    set index index + 1
  ]

  output-print ""
  output-print "Matriz de poblacion con aptitud:"
  output-print matrix:pretty-print-text matrizdepoblacion

  output-print ""
  output-print "Matriz de poblacion ordenada con aptitud:"
  set matrizdepoblacion sort-with [ l -> item sinapsis l ] matrix:to-row-list matrizdepoblacion
  set matrizdepoblacion matrix:from-row-list matrizdepoblacion
  output-print matrix:pretty-print-text matrizdepoblacion

  set error-generacion-old 100 ;; Inicializamos el error en un valor muy alto

  set matrizOrdenada matrizdepoblacion
  set matrizNew 0

  ;;set matriz matrix:to-row-list matrix:from-row-list n-values sinapsis [n-values crom [ -> random 2]]
  ;;set matriz2 matrix:to-row-list matrix:from-row-list n-values sinapsis [n-values crom [ -> random 2]]

  ;;;output-print matriz


end

to show-matrix
  ;output-print ""
  ;output-print "Matriz de aptitud"
  file-open "poblacion.txt"
  set i 0
  while[i < tam_poblacion]
  [
    let line file-read-line
    ;output-print line
    set i i + 1
  ]
  file-close
end


to setup-nodes                                          ;; PROCEDIMIENTO QUE SETUP LOS NODOS
  create-nodos-entrada 1 [                              ;; CREA UN NODO DE TIPO NODO-ENTRADA
    setxy -10 -2
    set nodo-entrada-1 self
  ]
  create-nodos-entrada 1 [                              ;; CREA UN NODO DE TIPO NODO-ENTRADA
    setxy -10 2
    set nodo-entrada-2 self
  ]
  ask nodos-entrada [ set activacion random 2 ]         ;; ASIGNA UN VALOR RANDOM A LOS NODOS DE ENTRADA PUEDE SER 0/1
  create-nodos-ocultos 1 [ setxy 0 -10 ]                ;; CREA LOS NODOS DE LA CAPA OCULTA
  create-nodos-ocultos 1 [ setxy 0 -5  ]                ;;
  create-nodos-ocultos 1 [ setxy 0  0  ]                ;;
  create-nodos-ocultos 1 [ setxy 0  5  ]                ;;
  create-nodos-ocultos 1 [ setxy 0  10 ]                ;;

  ask nodos-ocultos [                                   ;; SE LE ASIGNA UN VALOR ALEATORIO A LOS NODOS OCULTOS, SOLO ES TEMPORAL
    set activacion random 2
    set size 1.5
  ]

  create-nodos-salida 1 [                               ;; CREA LOS NODOS DE SALIDA Y SE LE ASIGNA UN VALOR TEMPORAL
    setxy 10 0
    set nodo-salida-1 self
    set activacion random 2
  ]

end

to recolor
  ask turtles [
    set color item (paso activacion) [ white yellow ]
  ]
  ask links [
    set thickness 0.05 * abs weight
    ifelse mostrar-pesos? [
      set label precision weight 4
    ] [
      set label ""
    ]
    ifelse weight > 0
      [ set color [ 255 0 0 196 ] ] ; transparent red
      [ set color [ 0 0 255 196 ] ] ; transparent light blue
  ]
end


;;;                               ;;;
;;; PROCEDIMIENTOS DE PROPAGACION ;;;
;;;                               ;;;

to propagar
  ask nodos-ocultos [ set activacion nueva-activacion ]
  ask nodos-salida [ set activacion nueva-activacion ]
  recolor
end

to propagar-test
  ask nodos-ocultos [ set activacion nueva-activacion ]
  ask nodos-salida [ set activacion nueva-activacion ]
  recolor
end

to-report nueva-activacion  ;; node procedure
  report sigmoidal sum [ [ activacion ] of end1 * weight ] of my-in-links
end

;;;
;;; Procediemietos matematicos
;;;

to-report sigmoidal [input]
  report 1 / (1 + e ^ (- input))
end

to-report paso [input]
  report ifelse-value (input > 0.5) [ 1 ] [ 0 ]
end

to-report aproximaciones [valarr tampob]
  ;print valarr
  set lista array:to-list valarr
  set egmax max lista
  set egmin min lista
  set i 0
  set k 0
  set arraysize array:length valarr
  set arraysize arraysize - 1
  let arregloA array:from-list n-values tampob [0]
  ;;print arraysize
  ;;output-print ""
  ;;output-print "Valores con el nuevo rango"

  while[i <= arraysize]
  [
    set k array:item valarr i
    set aproximacion1 (k - egmin) * (b - a)
    ;;print aproximacion1
    set aproximacion2  (aproximacion1) / (egmax - egmin)
    ;;print aproximacion2
    set aproximacion3 (aproximacion2) + a


    array:set arregloA i aproximacion3
    ;;output-print aproximacion3

    set i i + 1
  ]
  report arregloA
end

;;;                                                   ;;;
;;; PROCEDIMIENTOS DE ENTRENAMIENTO DE LOS INDIVIDUOS ;;;
;;;                                                   ;;;

to train



end

;;;                            ;;;
;;; PROCEDIMIENTOS MISCELANEOS ;;;
;;;                            ;;;
to-report sort-with [ key lst ]
  report sort-by [ [x y] -> (runresult key x) < (runresult key y) ] lst
end

to-report random-entre [ minrange maxrange ]
  report minrange + random-float (maxrange - minrange)
end

to-report calcular_crom [v1 v2 pc]
  set res abs v2 - v1
  set res res * pc
  set res ln res
  set res (res)/(ln 2)
  report round res
end

to-report evalua [matrizpob tampob tamcrom]

  set i 0
  set j tamcrom - 1
  set k 0
  set aux 0
  let arreglo array:from-list n-values tampob [0]
  ;;output-print ""
  ;;output-print "Valores originales"

  while[i <= tampob - 1]
  [
    while[j >= 0]
    [
      set aux matrix:get matrizpob i j
      set aux aux * 2 ^ k
      set j j - 1
      set k k + 1
      set suma suma + aux
    ]
    ;;print i
    array:set arreglo i suma
    ;;output-print suma
    set j tamcrom - 1
    set i i + 1
    set k 0
    set suma 0
  ]

  report arreglo
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to aparear

  set i 0
  set j 0
  set k 0
  set j tam_poblacion / 2
  set j j - 1
  set puc sinapsis / 2
  set puc round puc
  set k tam_poblacion / 2
  set matrizOld matrizOrdenada

  set cont 0
  while[i < j]
  [
    while[cont < puc]
    [
      ;print k
      ;print matrix:get matrizOrdenada i cont
      ;print j
      ;print cont
      matrix:set matrizOrdenada k cont matrix:get matrizOrdenada i cont
      matrix:set matrizOrdenada k + 1 cont matrix:get matrizOrdenada j cont
      set cont cont + 1
    ]
    set cont 0
    set puc sinapsis / 2
    set puc round puc
    set puc puc
    while[puc < sinapsis]
    [
      ;print k
      ;print puc
      ;print ""
      ;print k + 1
      ;print puc
      ;print ""

      ;print i
      ;print puc
      ;print ""
      ;print j
      ;print puc
      ;print ""
      matrix:set matrizOrdenada k + 1 puc matrix:get matrizOrdenada i puc
      matrix:set matrizOrdenada k puc matrix:get matrizOrdenada j puc
      set puc puc + 1
    ]
    set cont 0
    set i i + 1
    set j j - 1
    set k k + 2
    ;;if k = tam_poblacion [set i tam_poblacion]
    set puc sinapsis / 2
    set puc round puc
  ]
  ;print i
  ;print ""
  ;print j


  ;output-print ""
  output-print "Matriz con apareaciones:"
  output-print matrix:pretty-print-text matrizOrdenada ;;ESTA ES LA MATRIZ ORDENADA DE MAYOR A MENOR Y CON LOS HIJOS YA CREADOS

  set res matrix:dimensions matrizOrdenada ;;POBLACION A MUTAR
  set res item 0 res ;;dimensiones de la matriz
  set resDos res + 1 ;;variables temporales mas 1 ya que la funcion random solo genera valores estrictamente menores que el valor dado
  set cromToMute sinapsis + 1 ;;idem ^
  set n random resDos ;;NUMERO DE INDIVIDUOS A MUTAR
  set valoresR random cromToMute ;;NUMERO DE GENES A MUTAR (NUMERO DE VALORES DENTRO DEL CROMOSOMA)

  let matrizOrdenadaTMP matrix:to-column-list matrizOrdenada
  set matrizOrdenadaTMP matrix:from-column-list remove-item sinapsis matrizOrdenadaTMP

  ;print res
  ;print n
  ;print sinapsis

  set matrizOrdenadaTMP mutaciones matrizOrdenadaTMP res n sinapsis valoresR
  ifelse matrizOrdenadaTMP != 0
  [
    set matrizOrdenada matrizOrdenadaTMP
    output-print "Matriz con mutaciones"
    output-print matrix:pretty-print-text matrizOrdenada

    set index 0
    while[index < tam_poblacion]
    [
      let index2 0
      set columnaH matrix:get-row matrizOrdenada index

      setup-enlaces

      let xortable1 (list 0 1 0 1)
      let xortable2 (list 0 0 1 1)
      let salida-esperada (list 0 1 1 0)

      let xorcounter 0
      set error-c-medio array:from-list n-values 4 [0]

      ;;INICIA REPETICION PARA CADA FILA DE LA TABLA XOR, PRIMERO 00, 01, 10, 11
      while [xorcounter < 4]
      [
        ask nodo-entrada-1 [ set activacion item xorcounter xortable1 ]
        ask nodo-entrada-2 [ set activacion item xorcounter xortable2 ]
        propagar
        let answer item xorcounter salida-esperada
        ;print item xorcounter xortable2
        ;print item xorcounter xortable1
        ;print answer
        ;print ""
        ask nodo-salida-1 [
          set err (answer - activacion)
          set res err ^ 2
          array:set error-c-medio xorcounter res
          ;print activacion
          ;print err
          ;print ""
        ]
        set xorcounter xorcounter + 1
      ]
      ;;FINALIZA REPETICION

      set error-c-medio array:to-list error-c-medio
      set res sum error-c-medio
      set res res / 4
      ;print res
      ;print ""
      set error-generacion res
      ;print columnah
      ;print res
      set columnaH insert-item sinapsis columnaH res
      ;;output-print columnaH

      matrix:set-row matrizdepoblacion index columnaH
      set index index + 1
    ]

    output-print ""
    output-print "Matriz de poblacion con mutaciones ordenada con aptitud:"
    set matrizdepoblacion sort-with [ l -> item sinapsis l ] matrix:to-row-list matrizdepoblacion
    set matrizdepoblacion matrix:from-row-list matrizdepoblacion
    output-print matrix:pretty-print-text matrizdepoblacion
    set error-generacion matrix:get matrizdepoblacion 0 sinapsis


    ifelse error-generacion < error-generacion-old
    [
      set matrizOrdenada matrizdepoblacion
      set columnaH matrix:get-row matrizOrdenada 0
      setup-enlaces
      propagar
      set error-generacion-old error-generacion

      if error-generacion-old < 0.09
      [
        tick
        stop
      ]
    ]
    [
      set matrizOrdenada matrizOld
    ]


  ]
  [
    ;print "Ningun individuo ha recibido mutaciones"
  ]

 set generaciones generaciones + 1
 tick
end


to aparear2
  set generaciones 0
  while[generaciones < generaciones-a-evolucionar]
  [
  set i 0
  set j 0
  set k 0
  set j tam_poblacion / 2
  set j j - 1
  set puc sinapsis / 2
  set puc round puc
  set k tam_poblacion / 2
  set matrizOld matrizOrdenada

  set cont 0
  while[i < j]
  [
    while[cont < puc]
    [
      ;print k
      ;print matrix:get matrizOrdenada i cont
      ;print j
      ;print cont
      matrix:set matrizOrdenada k cont matrix:get matrizOrdenada i cont
      matrix:set matrizOrdenada k + 1 cont matrix:get matrizOrdenada j cont
      set cont cont + 1
    ]
    set cont 0
    set puc sinapsis / 2
    set puc round puc
    set puc puc
    while[puc < sinapsis]
    [
      ;print k
      ;print puc
      ;print ""
      ;print k + 1
      ;print puc
      ;print ""

      ;print i
      ;print puc
      ;print ""
      ;print j
      ;print puc
      ;print ""
      matrix:set matrizOrdenada k + 1 puc matrix:get matrizOrdenada i puc
      matrix:set matrizOrdenada k puc matrix:get matrizOrdenada j puc
      set puc puc + 1
    ]
    set cont 0
    set i i + 1
    set j j - 1
    set k k + 2
    ;;if k = tam_poblacion [set i tam_poblacion]
    set puc sinapsis / 2
    set puc round puc
  ]
  ;print i
  ;print ""
  ;print j


  ;output-print ""
  output-print "Matriz con apareaciones:"
  output-print matrix:pretty-print-text matrizOrdenada ;;ESTA ES LA MATRIZ ORDENADA DE MAYOR A MENOR Y CON LOS HIJOS YA CREADOS

  set res matrix:dimensions matrizOrdenada ;;POBLACION A MUTAR
  set res item 0 res ;;dimensiones de la matriz
  set resDos res + 1 ;;variables temporales mas 1 ya que la funcion random solo genera valores estrictamente menores que el valor dado
  set cromToMute sinapsis + 1 ;;idem ^
  set n random resDos ;;NUMERO DE INDIVIDUOS A MUTAR
  set valoresR random cromToMute ;;NUMERO DE GENES A MUTAR (NUMERO DE VALORES DENTRO DEL CROMOSOMA)

  let matrizOrdenadaTMP matrix:to-column-list matrizOrdenada
  set matrizOrdenadaTMP matrix:from-column-list remove-item sinapsis matrizOrdenadaTMP

  ;print res
  ;print n
  ;print sinapsis

  set matrizOrdenadaTMP mutaciones matrizOrdenadaTMP res n sinapsis valoresR
  ifelse matrizOrdenadaTMP != 0
  [
    set matrizOrdenada matrizOrdenadaTMP
    output-print "Matriz con mutaciones"
    output-print matrix:pretty-print-text matrizOrdenada

    set index 0
    while[index < tam_poblacion]
    [
      let index2 0
      set columnaH matrix:get-row matrizOrdenada index

      setup-enlaces

      let xortable1 (list 0 1 0 1)
      let xortable2 (list 0 0 1 1)
      let salida-esperada (list 0 1 1 0)

      let xorcounter 0
      set error-c-medio array:from-list n-values 4 [0]

      ;;INICIA REPETICION PARA CADA FILA DE LA TABLA XOR, PRIMERO 00, 01, 10, 11
      while [xorcounter < 4]
      [
        ask nodo-entrada-1 [ set activacion item xorcounter xortable1 ]
        ask nodo-entrada-2 [ set activacion item xorcounter xortable2 ]
        propagar
        let answer item xorcounter salida-esperada
        ;print item xorcounter xortable2
        ;print item xorcounter xortable1
        ;print answer
        ;print ""
        ask nodo-salida-1 [
          set err (answer - activacion)
          set res err ^ 2
          array:set error-c-medio xorcounter res
          ;print activacion
          ;print err
          ;print ""
        ]
        set xorcounter xorcounter + 1
      ]
      ;;FINALIZA REPETICION

      set error-c-medio array:to-list error-c-medio
      set res sum error-c-medio
      set res res / 4
      ;print res
      ;print ""
      set error-generacion res
      ;print columnah
      ;print res
      set columnaH insert-item sinapsis columnaH res
      ;;output-print columnaH

      matrix:set-row matrizdepoblacion index columnaH
      set index index + 1
    ]

    output-print ""
    output-print "Matriz de poblacion con mutaciones ordenada con aptitud:"
    set matrizdepoblacion sort-with [ l -> item sinapsis l ] matrix:to-row-list matrizdepoblacion
    set matrizdepoblacion matrix:from-row-list matrizdepoblacion
    output-print matrix:pretty-print-text matrizdepoblacion
    set error-generacion matrix:get matrizdepoblacion 0 sinapsis


    ifelse error-generacion < error-generacion-old
    [
      set matrizOrdenada matrizdepoblacion
      set columnaH matrix:get-row matrizOrdenada 0
      setup-enlaces
      propagar
      set error-generacion-old error-generacion
    ]
    [
      set matrizOrdenada matrizOld
    ]


  ]
  [
    ;print "Ningun individuo ha recibido mutaciones"
  ]

 set generaciones generaciones + 1
 tick
  ]
end


to-report mutaciones [pob_mutar tam_ind cuantos tam_genes cuantosGenes]
  set i 0
  set j 0
  set k 0
  set valores 0
  ;;print "tamaño de la poblacion"
  ;;print tam_ind
  output-print "cuantos individuos van a mutar"
  output-print cuantos
  ;print k
  set k range tam_ind ;;CREANDO UN RANGO (LISTA) DEL TAMAÑO DE INDIVIDUOS TOTALES DE LA POBLACION
  set k shuffle k
  set lista n-of cuantos k ;;CREANDO UNA LISTA DE LOS CANDIDATOS A MUTAR, ESCOGIDOS DE LOS LISTA DE INDIVIDUOS TOTALES
  set cromM1 range tam_genes ;;CREANDO UN RANGO (LISTA) DEL TAMAÑO DE GENES TOTALES EN EL CROMOSOMA
  set cromM1 shuffle cromM1

  output-print "cuantos genes va a mutar"
  output-print cuantosGenes


  ifelse empty? lista OR cuantosGenes = 0
  [
    ;print "No mutara ningun individuo"
    report 0
  ]
  [
    set lista array:from-list lista
    ;;print "individuos a mutar"
    ;;print lista

    while[i < cuantos]
    [
      set cromM2 n-of cuantosGenes cromM1 ;;CREANDO UNA LISTA DEL NUMERO DE GENES A MUTAR, ESCOGIDOS DE LA LISTA DE GENES TOTALES
      set cromM2 shuffle cromM2
      set cromM2 array:from-list cromM2
      ;;print "genes a mutar"
      ;;print cromM2

      set aux matrix:get-row pob_mutar array:item lista i
      ;;print aux
      while[j < cuantosGenes]
      [
        ;print aux
        ;print array:item cromM2 j

        ifelse item array:item cromM2 j aux > 0
        [
          set aux replace-item array:item cromM2 j aux random-entre -5 10
        ]
        [
          set aux replace-item array:item cromM2 j aux random-entre -10 5
        ]


        set j j + 1
      ]
      matrix:set-row pob_mutar array:item lista i aux

      set i i + 1
      set j 0
    ]
    report pob_mutar

  ]



end

to-report respuesta-esperada
  let x [ activacion ] of nodo-entrada-1 = 1
  let y [ activacion ] of nodo-entrada-2 = 1
  report ifelse-value run-result
    (word "x " "xor" " y") [ 1 ] [ 0 ]
end


to test
  let resultado resultado-de-entrada input-1 input-2
  let correct? ifelse-value (resultado = respuesta-esperada) [ "correcto" ] [ "incorrecto" ]
  user-message (word
    "La salida esperada para " input-1 " " "XOR" " " input-2 " es " respuesta-esperada ".\n\n"
    "La red reportó: " resultado ", lo cual es " correct? ".")
end

to-report resultado-de-entrada [n1 n2]
  ask nodo-entrada-1 [ set activacion n1 ]
  ask nodo-entrada-2 [ set activacion n2 ]
  propagar
  report paso [ activacion ] of one-of nodos-salida
end

to setup-enlaces
  let node 0
  let link-no 2
  let index-link 0

  while[node < 2]
  [
    set link-no 2
    while[link-no < 7]
    [
      ask link node link-no [set weight item index-link columnaH]
      set link-no link-no + 1
      set index-link index-link + 1
    ]
    set node node + 1
  ]

  set node 2
  set link-no 7
  while[node < 7]
  [
    ask link node link-no [set weight item index-link columnaH]
    set node node + 1
    set index-link index-link + 1
  ]
end

to setup-enlaces-test
  let node 0
  let link-no 2
  let index-link 0

  while[node < 2]
  [
    set link-no 2
    while[link-no < 7]
    [
      ask link node link-no [set weight random 2]
      set link-no link-no + 1
      set index-link index-link + 1
    ]
    set node node + 1
  ]

  set node 2
  set link-no 7
  while[node < 7]
  [
    ask link node link-no [set weight random 2]
    set node node + 1
    set index-link index-link + 1
  ]
end


to setup-enlaces-1
  set i 0
  connect-all nodos-entrada nodos-ocultos
  connect-all nodos-ocultos nodos-salida
end

to connect-all [ nodes1 nodes2 ]
  ask nodes1 [
    create-links-to nodes2 [
      ;;;output-print "heya"
      ;;;output-print columnaH
      set weight item i columnaH
      ;;print item i columnaH
      ;;;output-print item i columnaH
      set i i + 1
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
1426
13
1863
451
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

INPUTBOX
21
83
71
143
a
-1.0
1
0
Number

INPUTBOX
79
85
129
145
b
1.0
1
0
Number

INPUTBOX
137
85
292
145
p
100.0
1
0
Number

INPUTBOX
20
13
175
73
tam_poblacion
9.0
1
0
Number

SWITCH
1430
459
1571
492
mostrar-pesos?
mostrar-pesos?
0
1
-1000

INPUTBOX
144
560
299
620
generaciones-a-evolucionar
900.0
1
0
Number

CHOOSER
407
468
545
513
input-1
input-1
0 1
1

CHOOSER
409
519
547
564
input-2
input-2
0 1
0

BUTTON
232
468
295
501
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
516
295
549
NIL
aparear
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
295
10
1408
451
11

PLOT
22
156
292
452
Generaciones/error
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot error-generacion-old"

MONITOR
794
520
921
565
NIL
error-generacion-old
17
1
11

BUTTON
412
572
475
605
NIL
test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
864
579
921
624
salida
[precision activacion 2] of one-of nodos-salida
17
1
11

MONITOR
832
467
919
512
NIL
generaciones
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

hidden-node
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

input-node
false
0
Circle -7500403 true true 0 0 300

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

output-node
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
