;;; pyim-hyly.el --- Some zyoy dicts for pyim

;; * Header
;; Copyright (C) 2016 Suk Honzeon <sukhonzeon@gmail.com>

;; Author: Suk Honzeon <sukhonzeon@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((pyim "1.0"))
;; Keywords: convenience, Chinese, pinyin, input-method, complete

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;;;###autoload
(defun pyim-hyly-enable ()
  "Add hyly to pyim."
  (interactive)
  (let* ((dir (file-name-directory
               (locate-library "pyim-hyly.el")))
         (file (concat dir "pyim-hyly-min.pyim")))
    (when (file-exists-p file)
      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(:name "hyly-elpa" :file ,file :elpa t))
        (message "pyim 没有安装，pyim-hyly 启用失败。")))))

;; * Footer

(provide 'pyim-hyly)

;;; pyim-hyly.el ends here
