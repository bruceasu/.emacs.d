- [pyim-hyly README](#orgd680463)
  - [简介](#orgd08e8ad)
  - [安装和使用](#org1ee99e2)


<a id="orgd680463"></a>

# pyim-hyly README


<a id="orgd08e8ad"></a>

## 简介

pyim-hyly 是 pyim 的一个胡言乱语词库。


<a id="org1ee99e2"></a>

## 安装和使用

1.  配置melpa源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET pyim RET
3.  在emacs配置文件中（比如: ~/.emacs）添加如下代码：
	(require 'pyim)
	(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
	(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
	(setq default-input-method "pyim")
    (require 'pyim-hyly)
    (pyim-hyly-enable)

