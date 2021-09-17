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
  set-default-shape nodos-entrada "input_node"              ;; ESTABLECE LAS FORMAS DE LOS NODOS
  set-default-shape nodos-salida "output-node"              ;;
  set-default-shape nodos-ocultos "hidden-node"             ;;
  set-default-shape links "small-arrow-shape"               ;;
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

;;
;; PROCEDIMIENTO QUE CREA LOS NODOS ;;
;;

to setup-nodes                                          
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

to-report target-answer
  let x [ activacion ] of nodo-entrada-1 = 1
  let y [ activacion ] of nodo-entrada-2 = 1
  ;; run-result will interpret target-function as the appropriate boolean operator;;;;;;;;;;;;;;
  report ifelse-value run-result
    (word "x " "xor" " y") [ 1 ] [ 0 ]
end

;; test runs one instance and computes the output
to test
  let resultado resultado-de-entrada input-1 input-2
  let correct? ifelse-value (resultado = target-answer) [ "correcto" ] [ "incorrecto" ]
  user-message (word
    "La salida esperada para " input-1 " " "XOR" " " input-2 " es " target-answer ".\n\n"
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
