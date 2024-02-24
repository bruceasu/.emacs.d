# SUBMODULE
## ADD SUBMODULE

```bash
#语法： git submodule add [-b <branch>] [-f|--force] [--name <name>] [--reference <repository>] [--] <repository> [<path>]
```

```bash
git submodule add  https://github.com/manateelazycat/sort-tab.git  site-lisp/sort-tab
```

## First time init after clone
```bash
#1. checkout
git clone --recurse-submodules git@github.com:bruceasu/.emacs.d
#2. or
git clone git@github.com:bruceasu/.emacs.d
# then
git submodule init
git submodule update
# 3. or
git clone git@github.com:bruceasu/.emacs.d
git submodule update --init 
git submodule update --init --recursive
```
## Pull new version of submodule
### single
```
git submodule update --remote site-lisp/sort-tab
```
### multiply submodules
```
git submodule foreach 'git pull origin master'
```

# DELETE SUBMODULE
```
git submodule deinit -f site-lisp/sort-tab
```
then edit the .gitmodules to remove the submodule config 
then edit the .git/config to remove the submodule config 
and the also delete `rm -fr.git/modules/site-lisp/sort-tab`

# SUBTREE
## ADD SUBTREE
```bash
#optional
git remote add plugin-repo-name https://github.com/manateelazycat/sort-tab.git
# add
git subtree add --prefix=site-lisp/sort-tab plugin-repo-name_or_git_url master --squash
```

## UDPATE SUBTREE
`git subtree pull --prefix=site-lisp/sort-tab master --squash`
## DELETE SUBTREE
`git rm -fr site-lisp/sort-tab`

## CHOISE
大家可以根据实际情况进行选择使用，
如果只是依赖一个模块，subtree可能更简单一些，
如果主项目依赖多个子项目，submodule才是最好的选择。