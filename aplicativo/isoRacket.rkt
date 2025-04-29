#lang racket

(require web-server/servlet
         web-server/servlet-env)

;; Critérios e Perguntas
(define criterios-perguntas
  '(("Adequação à tarefa" "O site facilita a conclusão da(s) sua(s) principal(is) tarefa(s) ou objetivo(s) ao visitá-lo, sem exigir passos desnecessários ou informações irrelevantes?")
    ("Auto-descritividade" "É fácil entender o propósito de cada elemento (menus, botões, links) e saber onde você está dentro do site e o que está acontecendo (feedback) após cada ação realizada?")
    ("Conformidade com expectativas" "O site funciona de maneira consistente e previsível, seguindo convenções comuns da web (por exemplo, o logo leva à página inicial, a busca está no topo)? O comportamento geral do site corresponde ao que você esperaria?")
    ("Capacidade de aprendizado" "É fácil para um novo usuário aprender a navegar e utilizar as funcionalidades principais do site rapidamente, sem precisar de muita ajuda externa ou instruções complexas?")
    ("Controle do usuário" "Você sente que tem controle sobre a interação com o site, podendo iniciar, interromper ou reverter ações facilmente (como voltar para a página anterior, cancelar um processo ou corrigir dados em um formulário)?")
    ("Tolerância a erros" "O site ajuda a evitar que você cometa erros (por exemplo, com instruções claras ou validação de dados em formulários)? Caso ocorra um erro, ele é comunicado de forma clara e o site oferece ajuda ou sugestões para corrigi-lo facilmente, sem perder seu progresso?")
    ("Adequação individualizável" "O site oferece opções para adaptar a interface ou o conteúdo às suas necessidades ou preferências individuais (por exemplo, opções de acessibilidade como tamanho de fonte, escolha de idioma, personalização de painel de controle, salvar favoritos)?")))

;; Formulário inicial
(define (build-form)
  `(html
    (head
     (title "Avaliação de Usabilidade - DrRacket")
     (style "
        body { font-family: Arial, sans-serif; padding: 20px; max-width: 700px; margin: auto; }
        input[type='number'] { width: 60px; padding: 5px; }
        button, input[type='submit'] {
          padding: 10px 20px;
          background-color: #007BFF;
          color: white;
          border: none;
          border-radius: 4px;
          cursor: pointer;
        }
        .form-section { margin-bottom: 20px; }
     "))
    (body
     (h1 "Avaliação de Usabilidade (ISO 9241-10)")
     (form ([action "/submit"] [method "post"])
       ,@(for/list ([cp criterios-perguntas])
           `(div ([class "form-section"])
             (p ,(cadr cp))
             (input ([type "number"] [name ,(car cp)] [min "1"] [max "5"] [required "required"]))))
       (br)
       (input ([type "submit"] [value "Enviar Avaliação"]))))))

;; Resultado com gráfico e botão
(define (process-form req)
  (define bindings (request-bindings/raw req))
  (define binding-alist
    (for/list ([b bindings])
      (cons (bytes->string/utf-8 (binding-id b))
            (bytes->string/utf-8 (binding:form-value b)))))

  ;; Corrigido: só inclui números válidos
  (define notas
    (filter number?
            (for/list ([cp criterios-perguntas])
              (let* ([criterio (car cp)]
                     [valor-str (cdr (assoc criterio binding-alist))])
                (string->number valor-str)))))

  ;; Média corrigida para garantir cálculo de ponto flutuante
  (define media (exact->inexact (/ (apply + notas) (length notas))))

  (define criterios-str (map car criterios-perguntas))
  (define notas-str (map number->string notas))

  `(html
    (head
     (title "Resultados")
     (script ([src "https://cdn.jsdelivr.net/npm/chart.js"]) "")
     (style "
        body { font-family: Arial, sans-serif; padding: 20px; max-width: 800px; margin: auto; text-align: center; }
        canvas { max-width: 400px; margin: auto; }
        button {
          padding: 10px 20px;
          background-color: #007BFF;
          color: white;
          border: none;
          border-radius: 4px;
          cursor: pointer;
          margin-top: 20px;
        }
        ul { text-align: left; margin: auto; max-width: 500px; }
     "))
    (body
     (h1 "Resultados da Avaliação")
     (p ,(format "Média Geral: ~a" media))
     (h2 "Notas por Critério:")
     (ul
      ,@(for/list ([cp criterios-perguntas] [n notas])
          `(li ,(format "~a: ~a" (car cp) n))))
     (br)
     (canvas ([id "graficoNotas"] [width "400"] [height "400"]))
     (script
      ,(string-append
        "const ctx = document.getElementById('graficoNotas');"
        "new Chart(ctx, {type: 'pie', data: {"
        "labels: ["
        (string-join (map (λ(s) (format "\"~a\"" s)) criterios-str) ", ")
        "],"
        "datasets: [{"
        "label: 'Notas por Critério',"
        "data: [" (string-join notas-str ", ") "],"
        "backgroundColor: ['#007bff', '#28a745', '#ffc107', '#dc3545', '#17a2b8', '#6610f2', '#6c757d']"
        "}]}, options: {responsive: true}});"))
     (br)
     (form ([action "/"] [method "get"])
       (button "Voltar para Avaliação")))))

;; Servlet
(define (start req)
  (define path (url-path (request-uri req)))
  (define path-str (string-join (map path/param-path path) "/"))
  (cond
    [(or (equal? path-str "") (equal? path-str "/"))
     (response/xexpr (build-form))]
    [(equal? path-str "submit")
     (response/xexpr (process-form req))]
    [else
     (response/xexpr
      `(html
        (head (title "404 - Página Não Encontrada"))
        (body
         (h1 "404 - Página Não Encontrada")
         (p "O caminho solicitado não existe.")
         (form ([action "/"] [method "get"])
           (button "Voltar à Página Inicial")))))]))

;; Rodar no navegador
(serve/servlet start
               #:launch-browser? #t
               #:port 8000
               #:servlet-path "/"
               #:servlet-regexp #rx"")
