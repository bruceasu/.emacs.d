#!/bin/bash

# 检查是否提供了子模块名称参数
if [ -z "$1" ]; then
  echo "请提供要删除的子模块名称。"
  echo "用法: $0 <子模块名称>"
  exit 1
fi

SUBMODULE=$1

# 检查子模块是否存在
if [ ! -d ".git/modules/$SUBMODULE" ]; then
  echo "子模块 $SUBMODULE 不存在。"
  exit 1
fi

# 删除子模块条目
git submodule deinit -f -- "$SUBMODULE"
# 删除 .gitmodules 文件中的条目
git config -f .gitmodules --remove-section submodule.$SUBMODULE

# 删除 .git/config 文件中的条目
git config --remove-section submodule.$SUBMODULE

git rm -fr "$SUBMODULE"

# 删除子模块的残余文件
rm -rf ".git/modules/$SUBMODULE"
rm -rf "$SUBMODULE"

git add .gitmodules

# 提交更改
git commit -m "Removed submodule $SUBMODULE"

echo "子模块 $SUBMODULE 已成功删除。"
