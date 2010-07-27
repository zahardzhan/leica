;;; -*- mode: clojure; coding: utf-8 -*-

(ns test.sketch
  (:use clojure.test hooks)
  (:use :reload leica)
  (:require [async.http.client :as HTTP]
            [clojure.contrib.http.agent :as http])
  (:import java.io.File))

(do
  (def e0 (make-environment))
  (doseq [line
          ;; ["http://files2.dsv-region.data.cod.ru/proxy/1/?WyJlOTMzNjc0ZjNhODA5ODAwNTcwMmM5Y2VjNzQyYTI4NSIsMTI4MDQ2NjU1NSwiXHUwNDIxXHUwNDQyXHUwNDM4XHUwNDNhXHUwNDM1XHUwNDQwXHUwNDRiLnJhciIsIng4Y1BYMG0zendFU0M5a2I4aXhsb2I2T1RwMkRoSjRia081WmJBOUpVVVwvSDBOcmNwaFJjZ3dnWTlId2pPOVptMEg2T2pGd3MyUStGM0wwR2FcLzVZWXhBb1ZaQ2U5ditlSFkwRHZ6VVpXZDkzM3NhZGZIUjhKQWRialZNbld2K0llc21wdzZIMmRzZUJ2UWhwSDQ2amRVZVVjRWcrR29aWnZobnZTZGpDQWJNPSJd"
          ;;  "http://files2.dsv-region.data.cod.ru/proxy/1/?WyI1OWY4MDU4NDljNGUzOGI1MWE0NzYzMjRlNDgxOWY5MiIsMTI4MDQ2NjU2NiwiM0QtQXJ0LVdhbGxwYXBlcnMucmFyIiwiUHNOS3BocUhoZ05mOVd4cjlZR1ZJT1Y3eDZSWDNcL1NCZ0h0UVBFV1pmSVwvdFdadjlZcytscERRVUE1elFGYXdEMlE2dWExK2xHTVwvYm5cL1wvY3dXMVJNa2hyMEQ4XC9ubUhUU2t1bWptaG9kN0diSXpsM3ZQZ25iVVwvcFpKdHpoS3Z1YVlxOXRcL25tdGNtWU9McjhKNUFhdllCWE9wRUhETG5GODBGY1BOOXcxNUk9Il0%3D"
          ;;  ]
          ["http://dsv.data.cod.ru/868417"
           "http://dsv.data.cod.ru/867518"
           "http://dsv.data.cod.ru/866756"
           "http://dsv.data.cod.ru/868967"]
          ]
    (make-download-agent line :environment e0
                         :path "/home/haru/Inbox")))

(comment
  (let [a1 (first (agents e0))
        a2 (second (agents e0))]
    (count (select (agents e0) :entirely-after a1 :order-by #(derefed % :precedence)
                   :where #(and (derefed % fail?) (same :service (deref a1) (deref %))))))
  
  (count (agents e0))
  (send-off (first (agents e0)) run)

  (doseq [a (agents e0)]
    (send-off a run))

  [(map #(derefed % state) (agents e0))
   (map #(derefed % fail-reason) (agents e0))
   (map agent-errors (agents e0))]

  (throw  (first (agent-errors (first (next (next (agents e0)))))))

  )
