(ns kixi.transcode
  (:require
   [clojure.data.json :as json]
   [applied-science.darkstar :as darkstar])
  (:import
   javax.imageio.ImageIO
   [java.awt RenderingHints]
   [java.nio.charset StandardCharsets]
   [java.io File FileOutputStream ByteArrayInputStream ByteArrayOutputStream]
   [org.apache.batik.anim.dom SAXSVGDocumentFactory]
   [org.apache.batik.transcoder TranscoderInput TranscoderOutput SVGAbstractTranscoder]
   [org.apache.batik.transcoder.image PNGTranscoder]))

(def svg-parser (SAXSVGDocumentFactory. "org.apache.xerces.parsers.SAXParser"))

(defn svg-string->document [s]
  (with-open [in (ByteArrayInputStream. (.getBytes s StandardCharsets/UTF_8))]
    (.createDocument svg-parser "file:///fake.svg" in)))

(defn- high-quality-png-transcoder []
  (proxy [PNGTranscoder] []
    (createRenderer []
      (let [add-hint (fn [hints k v] (.add hints (RenderingHints. k v)))
            renderer (proxy-super createRenderer)
            ;;hints    (.getRenderingHints renderer)
            hints (RenderingHints. RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY)]
        (doto hints
          (add-hint RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY)
          (add-hint RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BICUBIC)
          (add-hint RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_ON)
          (add-hint RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_QUALITY)
          (add-hint RenderingHints/KEY_DITHERING           RenderingHints/VALUE_DITHER_DISABLE)
          (add-hint RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_QUALITY)
          (add-hint RenderingHints/KEY_STROKE_CONTROL      RenderingHints/VALUE_STROKE_PURE)
          (add-hint RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_ON)
          (add-hint RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
        (.setRenderingHints renderer hints)
        renderer))))

(defn children [element]
  (let [c (.getChildNodes element)]
    (if (zero? (.getLength c))
      nil
      (map #(.item c %) (range (.getLength c))))))

(defn ->xml
  ([element]
   (->xml element true))
  ([element children?]
   (if (instance? org.w3c.dom.Text element)
     (.getWholeText element)
     (let [attrs (.getAttributes element)]
       (merge
        {:tag (keyword (.getLocalName element))
         :ns  (.getNamespaceURI element)}
        (when (and attrs (not (zero? (.getLength attrs))))
          {:attrs (into {}
                        (map (fn [i] (let [attr (.item attrs i)]
                                       [(keyword (.getName attr))
                                        (.getNodeValue attr)]))
                             (range (.getLength attrs))))})
        (when children?
          (when-let [c (children element)]
            {:content (mapv #(->xml % children?) c)})))))))

(defn- document-dimensions [doc]
  (-> doc ->xml :content first :attrs
      (select-keys [:width :height])
      (update :width parse-double)
      (update :height parse-double)))

(def transcoder-keys
  {:width      SVGAbstractTranscoder/KEY_WIDTH
   :height     SVGAbstractTranscoder/KEY_HEIGHT
   :max-width  SVGAbstractTranscoder/KEY_MAX_WIDTH
   :max-height SVGAbstractTranscoder/KEY_MAX_HEIGHT})

(defn vl-map->bytearray
  ([vl-chart-map]
   (vl-map->bytearray vl-chart-map {}))
  ([vl-chart-map {:keys [width scale]}]
   (with-open [out-stream (ByteArrayOutputStream.)]
     (let [svg-doc (-> vl-chart-map
                       json/json-str
                       darkstar/vega-lite-spec->svg
                       svg-string->document)
           {doc-width :width :as dimensions} (document-dimensions svg-doc)
           in (TranscoderInput. svg-doc)
           out (TranscoderOutput. out-stream)
           trans (high-quality-png-transcoder)]
       (cond
         scale
         (.addTranscodingHint trans (:width transcoder-keys) (float (* scale doc-width)))
         width
         (.addTranscodingHint trans (:width transcoder-keys) (float width)))
       (.transcode trans in out)
       (.toByteArray out-stream)))))

(defn byte-array-to-png [byte-array filename]
  (with-open [in-stream (ByteArrayInputStream. byte-array)]
    (let [image (ImageIO/read in-stream)]
      (ImageIO/write image "png" (File. filename)))))

;; ------------------------------------
  ;;; Examples ;;;
;; (def example-vega-lite-chart-map
;;   {:data {:values [{:a "A" :b 28}
;;                    {:a "B" :b 55}
;;                    {:a "C" :b 43}
;;                    {:a "D" :b 91}
;;                    {:a "E" :b 81}
;;                    {:a "F" :b 53}
;;                    {:a "G" :b 19}
;;                    {:a "H" :b 87}
;;                    {:a "I" :b 52}]}
;;    :encoding {:x {:axis {:labelAngle 0} :field "a" :type "nominal"}
;;               :y {:field "b" :type "quantitative"}}
;;    :mark "bar"})

;; ;; write vega-lite chart map to byte array
;; (-> example-vega-lite-chart-map
;;     vl-map->bytearray)

;; ;; write vega-lite chart map to png file and change size
;; (-> example-vega-lite-chart-map
;;     vl-map->bytearray
;;     (byte-array-to-png "example-vega-lite-chart.png"))

;; ;; write svg to png in xlsx

;; (require '[tablecloth.api :as tc])

;; (require '[kixi.large :as large])

;; (-> [{:plot-title "Chart title"
;;       ::large/sheet-name "Sheet 1"
;;       ::large/data (-> example-vega-lite-chart-map
;;                        :data
;;                        :values
;;                        tc/dataset)
;;       ::large/images      [{::large/image (-> example-vega-lite-chart-map
;;                                               vl-map->bytearray
;;                                               svg-document->png)}]}]
;;     large/create-workbook
;;     (large/save-workbook! "test.xlsx"))
