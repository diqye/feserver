## 多项目代理服务
用于在本地环境开发项目，解决多个项目服务互相依赖问题

- 本地文件服务
- 目录浏览
- https项目代理
- 接口环境

## 配置文件
```yaml
# 设置80需要 sudo 执行服务程序
port: 80

# 路由选项 - 更改即时生效
# 路由会从前向后匹配，直到匹配成功
routers:
- path: /adam/api
  rewrite: https://m.vipfengxiao.com/adam/api
  originHeader: diqye.com
- path: /api/gw
  rewrite: https://test3-www.vipfengxiao.com/api/gw
- path: /vipbclass
  rewrite: http://test3-www.vipfengxiao.com/vipbclass
- path: /classroom
  rewrite: http://172.23.98.166:8080/classroom
- path: /mobile
  rewrite: https://test3-m.vipfengxiao.com/mobile
- path: /static
  locationPath: /Users/diqye
- path: /js
  # 文件服务，如果文件夹内有index.html,diqye.html
  # 则渲染
  # 否则需要完整路径
  locationPath: /Users/diqye/js
# 匹配所有
- path: /
  # locationPath: /
```
文件放置`~/feserver.yaml`

## Mac 端工具下载
[feserver](./feserver)

