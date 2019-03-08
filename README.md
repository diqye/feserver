## 多项目代理服务
用于在本地环境开发项目，解决多个项目服务互相依赖问题

- 本地文件服务
- 目录浏览
- https项目代理
- 接口环境

## 配置文件
```yaml
port: 80

# 路由选项 - 更改即时生效
# 路由会从前向后匹配，直到匹配成功
routers:
  # 一般来说这里只需要一个本地运行的项目
- path: /vipbclass
  rewrite: http://localhost:8080/vipbclass
  # 这里添加个人自定义的目录 访问/home 浏览/目录
  # - path: /home
  # locationPath: /
# 匹配所有，联调什么环境使用什么域名
- path: /
  rewrite: https://www.vipfengxiao.com/
```
文件放置`~/feserver.yaml`

## Mac 端工具下载
[feserver](./feserver)

## 使用
1. 确保上文中的配置文件保存至 ~/feserver.yaml
2. 工具下载完成
3. 进入下载目录
4. 执行下面命令


```bash
chmod 777 feserver
nohup ./feserver &
```

## 使用pm2 如果报权限问题
```bash
➜  happstack-myproxy git:(master) ✗ which pm2
/usr/local/bin/pm2
➜  happstack-myproxy git:(master) ✗ chmod -R 777 /usr/local/bin/pm2
```
然后再用 `sudo pm2 start feserver`

