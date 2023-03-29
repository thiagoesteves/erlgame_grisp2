[![Erlant/OTP Release](https://img.shields.io/badge/Erlang-OTP--25.0-green.svg)](https://github.com/erlang/otp/releases/tag/OTP-25.0)

# Game webserver written in Erlang and running in GRiSP2
![Erlgame](/doc/erlgame_snake.png)

The app has the same core game written in [Erlang](https://github.com/thiagoesteves/erlgame) but it is deployed in the [GRiSP 2 board](https://github.com/grisp/grisp/wiki/GRiSP-2-Hardware-Quickstart).

The aim of this project is to study GRiSP 2 applications using webserver and also acquire know-how of the GRiSP development with Erlang as Kernel itself.


## Build Steps

The application is running in a specific version of OTP and the project specifies the OTP and rebar3 version using the [asdf program](https://asdf-vm.com/guide/getting-started.html). It is important to keep in mind that the OTP version locally must match the version that will be deployed. 

### Local compilation
The next command will download any package needed and also compile the erlang application:
```
$ rebar3 compile
````

### Deployment
The GRiSP 2 board support only a few OTP versions. It means that the rebar3_grisp plugin will download pre-compile toolchains to compile the application to the GRiSP target board. If you need a non-listed OTP version you will need some other steps to prepare the toolchain.

If you want to check the supported pre-build OTP toochain version:
```
$ rebar3 grisp package list         
===> GRiSP pre-built OTP versions for platform 'grisp2'
Version
23.3.4.9
23.3.4.11
24.3.4.2
24.3.4.8
25.0.3
25.2.3
````
For this project, we selected the OTP version `25.0.3`. It means the file `rebar.config` need to be set for this specific version and this info will be use during the deployment for compatibility checks.

During the development, I found very challenging to make the combination: `rebar3`, `OTP` and `toolchain` to work properly in the MAC OS, even using the asdf - [Deployment guide](https://github.com/grisp/grisp/wiki/Deploying-a-GRiSP-Application). In order to mitigate this problem, I added some steps in the Makefile to create a `image-builder` container using docker that will compile the app and generate the files in the `image/` folder.

Once the files are generate, you can just copy them to the SD-Card

#### Generate image builder
```
$ make docker.build
````
#### Deploy using the image builder
Before generating the deployment files, be sure you read all documentation about [deployment with wi-fi](https://github.com/grisp/grisp/wiki/Connecting-over-WiFI-and-Ethernet) and also setup all values accordingly with your environment:
```
$ make grisp.image
````
#### Copy files to the SD-Card (MAC OS)
```
$ cp -a image/ /Volumes/{VOLUME-NAME}/
````

## References

  * Grisp website: https://www.grisp.org/resources/
  * Grisp Quickstart: https://github.com/grisp/grisp/wiki/GRiSP-2-Hardware-Quickstart
  * Grisp Software: https://github.com/grisp/grisp