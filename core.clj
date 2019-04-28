(ns ImageDescriptor.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File]))

(def read-image
  "reads in a given image"
  (memoize (fn [filename]
  (let [file (File. filename)]
    (ImageIO/read file)))))

(defn save-image
  "saves a given image with the name and extension that is given"
  [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)))

(defn get-width
  "gets the width of a given image"
  [image]
  (.getWidth image))

(defn get-height
  "gets the height of a given image"
  [image]
  (.getHeight image))

(def new-image
  "creates a new 'blank' image with a given width and height"
  (memoize (fn [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB))))

(defn set-rgb
  [image x y value]
  (let [rgb (+ (bit-shift-left value 16)
               (bit-shift-left value 8)
               (bit-shift-left value 0))]
    (.setRGB image x y rgb)))



(def get-filter
  "returns the kirsh fiter corresponding to the given index"
  (memoize (fn [index]
  (let [filters [[[-1 0 1][-2 0 2][-1 0 1]] [[-2 -1 0][-1 0 1][0 1 2]] [[-1 -2 -1][0 0 0][1 2 1]] [[0 -1 -2][1 0 -1][2 1 0]] [[1 0 -1][2 0 -2][1 0 -1]] [[2 1 0][1 0 -1][0 -1 -2]] [[1 2 1][0 0 0][-1 -2 -1]] [[0 1 2][-1 0 1][-2 -1 0]]]]
  (nth filters index)))))



(defn apply-filter
  "applys the given filter to the given pixel of the given image"
  [image x y f]
  (let [values
        (into []
              ;for each row of the filter..
              (for [row (range 0 3)]
                (let [temp
                      (into []
                            ;for each column of the filter...
                            (for [col (range 0 3)]
                              ;get the corresponding pixel, grey val of the pixel, and the value at the current row and column of the filter
                              (let [pixel (.getRGB image (+ x (dec row)) (+ y (dec col)))
                                    grey (bit-and pixel 0xFF)
                                    fNum (nth (nth f row) col)]
                                ;return the product of the grey value and the value of the filter at the current row and column
                                (* grey fNum))))]
                  ;return the values calculated for the current row in a vector
                  temp)))
        ;return the sum of all 9 values
        sum (reduce + (flatten values))] sum))



(defn kirsh
  "takes an 8-bit greyscale image and an index and then applies the kirsh filter H(i) to the image"
  [filename index]
  (let [f (get-filter index)
        original (read-image filename)
        width (get-width original)
        height (get-height original)
        fin (new-image (- width 2) (- height 2))]
    ;for each x coordinate that the filter can be apllied to (not the border)
    (loop [x 1]
      (if(< x (- width 1))
        (do
         ;for each y coordinate that the filter can be apllied to (not the border)
         (loop [y 1]
           (if (< y (- height 1))
             (do
              ;apply the filter to the current pixel (add 127 for display purposes)
              (let [pixel (+ 127 (apply-filter original x y f))]
                ;clamp the pixel value between 0-255 and then set the value of the cooresponding pixel in the final image
                (if(< pixel 0)
                  (set-rgb fin (- x 1) (- y 1) 0)               ;(println "ID1: " id1)
               ;(println "ID2: " id2)
               ;(println values)
                  (if(> pixel 255)
                    (set-rgb fin (- x 1) (- y 1) 255)
                    (set-rgb fin (- x 1) (- y 1) pixel))))
              ;repeat with the next y coordinate
              (recur (+ y 1)))))
         ;repeat with the next x coordinate
         (recur (+ x 1)))))
    ;return the final image
    fin))



(defn normalise-hist
  "returns a normalised version of the given histogram"
  [hist]
        ;calculate the sum of the values in the histogram
  (let [sum (reduce + (vals hist))
        ;put the histogram in bin order
        histTemp (into (sorted-map) hist)
        ;normalise the histogram
        histNew (into [] (for [k histTemp] (float (/ (val k) sum))))]
    ;return the normalised histogram as a vector of values
    histNew))



(defn edge-magnitude-hist
  "returns a normalised histogram with 8 bins containing edge magnitudes for the given image"
  [filename]
  ;define local variables for the image of the filename given and the width and height of the image
  (let [image (read-image filename)
        width (get-width image)
        height (get-height image)
        ;define a local variable for the magnitude histogram
        histTemp (apply merge-with + {}
                    ;for each x coordinate that the filter can be applied to...
                    (for [x (range 1 (- width 1))]
                      (let [;define a local variable that is a collection of hashmaps indicating the direction of the edge of each pixel in the current column
                            temp
                            ;for each y coordinate that the filter can be applied to...
                              (for [y (range 1 (- height 1))]
                                  ;calculate the value given when each Kirsh filter is applied to the current pixel
                                  (let [v0 (apply-filter image x y (get-filter 0))
                                        v1 (apply-filter image x y (get-filter 1))
                                        v2 (apply-filter image x y (get-filter 2))
                                        v3 (apply-filter image x y (get-filter 3))
                                        v4 (* v0 -1)
                                        v5 (* v1 -1)
                                        v6 (* v2 -1)
                                        v7 (* v3 -1)
                                        values {0 v0, 1 v1, 2 v2, 3 v3, 4 v4, 5 v5, 6 v6, 7 v7}
                                        ;calculate the maximum value
                                        magnitude (val (apply max-key val values))
                                        ;calculate which bin the maximum magnitude belongs in on the histogram
                                        bin (int (Math/floor (* magnitude (/ 8 256.0))))]
                                    ;clamp the magnitude between 0-255
                                    (if(< bin 0)
                                      {0 1}
                                      (if(> bin 7)
                                        {7 1}
                                        {bin 1}))))
                             ;define a local variable which is the direction histogram for the current column
                              yHist (apply merge-with + {} temp)]
                        yHist)))
        ;normalise the histogram
        histM (normalise-hist histTemp)]
    ;return the normalised magnitude histogram
    histM))



(defn edge-direction-hist
  "returns a normalised histogram with 8 bins containing edge directions for the given image"
  [filename]
  ;define local variables for the image of the filename given and the width and height of the image
  (let [image (read-image filename)
        width (get-width image)
        height (get-height image)
        ;define a local variable for the direction histogram
        histTemp (apply merge-with + {}
                    ;for each x coordinate that the filter can be applied to...
                    (for [x (range 1 (- width 1))]
                      (let [;define a local variable that is a collection of hashmaps indicating the direction of the edge of each pixel in the current column
                             temp
                            ;for each y coordinate that the filter can be applied to...
                              (for [y (range 1 (- height 1))]
                                  ;calculate the value given when each Kirsh filter is applied to the current pixel
                                  (let [v0 (apply-filter image x y (get-filter 0))
                                        v1 (apply-filter image x y (get-filter 1))
                                        v2 (apply-filter image x y (get-filter 2))
                                        v3 (apply-filter image x y (get-filter 3))
                                        v4 (* v0 -1)
                                        v5 (* v1 -1)
                                        v6 (* v2 -1)
                                        v7 (* v3 -1)
                                        values {0 v0, 1 v1, 2 v2, 3 v3, 4 v4, 5 v5, 6 v6, 7 v7}
                                        ;calculate which filter gave the maximum value
                                        direction (key (apply max-key val values))]
                                    {direction 1}))
                             ;define a local variable which is the direction histogram for the current column
                              yHist (apply merge-with + {} temp)]
                        yHist)))
        ;normalise the histogram
        histD (normalise-hist histTemp)]
    ;return the normalised direction histogram
    histD))



(defn image-descriptor
  "returns a normalised image descriptor for the given image"
  [filename]
             ;define local variables for the width and height of the image, and also the magnitude and direction histograms of the image
             (let [image (read-image filename)
                   width (get-width image)
                   height (get-height image)
                   histM (edge-magnitude-hist filename)
                   histD (edge-direction-hist filename)
                   histTemp (apply merge-with + {}
                                   (for [x (range 0 (- width 1))]
                      (let [;define a local variable that is a collection of hashmaps indicating the direction of the edge of each pixel in the current column
                             temp
                            ;for each y coordinate that the filter can be applied to...
                              (for [y (range 0 (- height 1))]
                                  ;calculate the value given when each Kirsh filter is applied to the current pixel
                                  (let [pixel (bit-and (.getRGB image x y) 0xFF)
                                 bin (int (Math/floor (* pixel (/ 8 256.0))))]

                             ;update the intensity histogram
                             (if (> bin 7)
                              {7 1}
                              {bin 1})))
                             ;define a local variable which is the direction histogram for the current column
                              yHist (apply merge-with + {} temp)]
                        yHist)))
                   ;normalise the intensity histogram
                   histI (normalise-hist histTemp)
                   ;concatenate the magnitude, direction and intensity histograms to form the image descriptor
                   histMDI (into [] (concat histM histD histI))
                   ;normalised the image descriptor
                   id (into [] (map #(/ % (reduce + histMDI)) histMDI))]
               ;return the normalised image descriptor
               id))



(defn image-similarity
  "takes 2 images and returns a value between 0.0 and 1.0 indicating how similar the images are"
  [file1 file2]
                   ;calculate the image descriptors for each image
             (let [id1 (image-descriptor file1)
                   id2 (image-descriptor file2)
                   ;for each index of the image descriptors, get the minimum value of image1's and image2's descriptors
                   values (map min id1 id2)
                   ;calculate the similarity score by summing the minimum values
                   score (reduce + values)]
               ;return the score
               score))


(defn -main
  [& args]
  (image-similarity image1.jpg image2.jpg))




