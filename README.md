## 代理Server V3.0.0
开发联调过程，对于其他项目依赖，接口环境依赖,可使用本代理解决。

- 本地文件夹服务
- 本地mock支持
- http、https代理支持
- 对404、文件不存在有特殊处理
- 长链接支持(next 热更新)

## 安装下载
### 1. Mac 端工具下载
[feserver](./feserver)

### 2. 配置文件
文件放至 `~/feserver.yaml` 位置


feserver.yaml
```yaml
#本地服务端口 建议80
port: 80
# 路由会从前向后匹配，直到匹配成功
# 如果代理得到的是404,视为匹配失败，会继续向下走
routers:
- startWith: /vipbclass
  rewrite: http://localhost:8080/vipbclass
- startWith: /
  rewrite: https://www.vipfengxiao.com
```
### 3. 启动
```bash
cd your/download/path
chmod 777 feserver
nohup ./feserver &
```

#### 使用pm2 如果报权限问题
```bash
➜  happstack-myproxy git:(master) ✗ which pm2
/usr/local/bin/pm2
➜  happstack-myproxy git:(master) ✗ chmod -R 777 /usr/local/bin/pm2
```
然后再用 `sudo pm2 start feserver`

### 4. url访问
1. localhost/vipbclass/lessonlist 代理至 http://localhost:8080/vipblcass/lessonlist
2. localhost/gw/api/xxx 代理至 https://www.vipfengxiao.com/gw/api/xx
3. 配置文件即改即生效

## 当接口404走本地mock数据
```yaml
port: 80
routers:
- startWith: /vipbclass
  rewrite: http://localhost:8080/vipbclass
- startWith: /
  rewrite: https://www.vipfengxiao.com
- startWith: /
  locationPath: /Users/diqye/mockdir
```
实际流程:

1. 访问 localhost/gw/api/yourapi
2. 代理至 https://www.vipfengxiao.com/gw/api/youapi
3. 如404 寻找本地文件 /Users/diqye/mockdir/gw/api/yourapi/mock.json

## 当mock数据不存在走接口

```yaml
port: 80
routers:
- startWith: /vipbclass
  rewrite: http://localhost:8080/vipbclass
- startWith: /
  locationPath: /Users/diqye/mockdir
- startWith: /
  rewrite: https://www.vipfengxiao.com
```
实际流程:

1. 访问 localhost/gw/api/yourapi
2. 寻找本地文件 /Users/diqye/mockdir/gw/api/yourapi/mock.json
2. 如文件不存在 代理至 https://www.vipfengxiao.com/gw/api/youapi



