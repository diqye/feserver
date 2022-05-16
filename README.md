## feserver
简易反向代理开发工具：本地只需要启动需要的修改的项目，其它所依赖的项目皆代理到服务中，用以简化本地环境。

## 安装下载
### 1. Mac 端工具下载
执行下面命令
```shell
curl -sSL https://s.vipkidstatic.com/beeschool/fx-fe/01188310faea165e221c613449a90a1c.sh | sh
# 既而运行
feserver
```
### 2. 配置文件
上方命令会自动生成配置文件，位置位于: `~/feserver.yaml` 

feserver.yaml
```yaml
port: 80
#HTTPSPort: 443
#HTTPSCertificate: /private/key-cert.pem
#HTTPSKey: /private/key.pem
routers:
- startWith: /myproject
  rewrite: http://localhost:8080/mypath
- startWith: / #匹配所有
  rewrite: https://www.servername.com
```
运行逻辑：
  
1. client 访问 localhost/myproject/a/b/c
2. feserver 根据前缀/myproject匹配而推断出代理至 http://localhost:8080/mypath/a/b/c
    1. 若404则继续向下匹配而推断出代理至 https://www.servername.com/a/b/c
        - 若亦404则响应404
        - 否则响应之
      2. 否则响应之
3. 根据以上规则，不难做出本地最轻化环境
4. 配置文件修改即生效无需重启


### 3. 示例
当前正在开发admin-dashboard项目,本地只启动了一个admin-dashboard前端服务.按照流程需要先登陆才能进到我这个页面，而登陆相关的业务在admin-user项目下，不用担心无关的项目都在公司的测试服务环境即可。
```yaml
port: 80 # 支持IPv4,IPv6
HTTPSPort: 443
HTTPSCertificate: /priavte/key-cert.pem
HTTPSKey: /private/key.pem
routers:
- startWith: /admin-dashboard
  rewrite: http://localhost:8080/admin-dashboard
- startWith: /api/dashboard/ # 此配置意思是：以/api/dashboard/开头的路径走和我联条的某个后端的机器
  rewrite: http://192.168.1.x/xxx
- path: /
  rewrite: https://www.admindemo.com/
- startWith: / # 此配置意思是：如果贴近上方的配置的服务某个接口还未部署(404)，则可以走本地的mock数据
  locationPath: /private/mock/

```

