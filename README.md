## 代理Server V3.0.0
解决开发联调过程中，对于其他项目依赖，接口环境依赖

- 本地文件夹服务
- 本地mock支持
- http、https代理支持
- 对404、文件不存在有特殊处理
- 长链接支持(next环境支持)

## 安装下载
### 1. Mac 端工具下载
执行下面命令
```shell
curl -sSL https://s.vipkidstatic.com/beeschool/fx-fe/01188310faea165e221c613449a90a1c.sh | sh
```
下载后可直接运行 sudo feserver

### 2. 配置文件
文件位置: `~/feserver.yaml` 


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

### 5. https协议支持
配置文件中增加 

```yaml
port: 80
# https支持
HTTPSPort: 443
HTTPSCertificate: /Users/diqye/Downloads/https/key-cert.pem
HTTPSKey: /Users/diqye/Downloads/https/key.pem
```
完整示例

```yaml
port: 80
HTTPSPort: 443
HTTPSCertificate: /Users/diqye/Downloads/https/key-cert.pem
HTTPSKey: /Users/diqye/Downloads/https/key.pem
routers:
- startWith: /douzhuanxingyi
  rewrite: http://localhost:8080/douzhuanxingyi
- path: /postgresql
  rewrite: http://127.0.0.1:53946
- startWith: /react
  rewrite: http://localhost:3000
- startWith: /js
  locationPath: /Users/diqye
- startWith: /favicon.ico
  locationPath: /Users/diqye/Downloads/Untitled Diagram.png
- startWith: /
  rewrite: https://www.vipfengxiao.com
```
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



