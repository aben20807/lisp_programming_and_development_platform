(setq X nil)nil
(defvar X "aa")X
Xnil

(makunbound 'X)X
X
(defvar X "aa")X
X"aa"

(unintern "X")t
X
(defvar X "aa")X
X"aa"

(unintern "X")t
(unintern "X")nil
