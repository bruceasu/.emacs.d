;;; pyim-zyoydict.el --- Some zyoy dicts for pyim

;; * Header
;; Copyright (C) 2016 Suk Honzeon <sukhonzeon@gmail.com>

;; Author: Suk Honzeon <sukhonzeon@gmail.com>
;; URL: 
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
;; * pyim-zyoydict README                         :README:doc:

;; ** 简介
;; pyim-zyoydict 是 pyim 的一个真言奥义词库.

;; ** 安装和使用
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET pyim-zyoydict RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：
;;    #+BEGIN_EXAMPLE
;;    (require 'pyim-zyoydict)
;;    (pyim-zyoydict-enable) ;
;;    #+END_EXAMPLE

;;; Code:
;; * 代码                                                               :code:

;;;###autoload
(defun pyim-zyoydict-enable ()
  "Add zyoydict to pyim."
  (interactive)
  (let* ((dir (file-name-directory
               (locate-library "pyim-zyoydict.el")))
         (file (concat dir "pyim-zyoydict.pyim")))
    (when (file-exists-p file)
      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(:name "zyoydict-elpa" :file ,file :elpa t))
        (message "pyim 没有安装，pyim-zyoydict 启用失败。")))))

;; * Footer

(provide 'pyim-zyoydict)

;;; pyim-zyoydict.el ends here
