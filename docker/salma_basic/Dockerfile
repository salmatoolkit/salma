FROM debian:jessie
COPY etc/apt/sources.list /etc/apt/sources.list
RUN apt-get update && apt-get install -y wget less
RUN mkdir /downloads

# JAVA
WORKDIR /downloads
RUN wget --no-cookies --no-check-certificate \
  --header "Cookie: oraclelicense=accept-securebackup-cookie" \
  http://download.oracle.com/otn-pub/java/jdk/8u65-b17/jdk-8u65-linux-x64.tar.gz
RUN tar xzf jdk-8u65-linux-x64.tar.gz
RUN mkdir /usr/lib/jvm
RUN mv jdk1.8.0_65 /usr/lib/jvm
RUN ln -s /usr/lib/jvm/jdk1.8.0_65/ /usr/lib/jvm/java-8-oracle
RUN update-alternatives --install "/usr/bin/java" "java" "/usr/lib/jvm/java-8-oracle/bin/java" 1
ENV JAVA_HOME=/usr/lib/jvm/jdk1.8.0_65

RUN rm jdk-8u65-linux-x64.tar.gz

# ECLIPSE
RUN mkdir /opt/eclipseclp
RUN wget --no-cookies --no-check-certificate \
  --header "Cookie: oraclelicense=accept-securebackup-cookie" \
  http://sunsite.informatik.rwth-aachen.de/ftp/pub/mirror/ECLiPSe-CLP/Current/6.1_216_x86_64_linux/eclipse_basic.tgz
WORKDIR /opt/eclipseclp
RUN tar xzf /downloads/eclipse_basic.tgz
RUN rm /downloads/eclipse_basic.tgz
RUN printf "\n" | ./RUNME
ENV ECLIPSEDIR=/opt/eclipseclp
ENV PATH=$PATH:$ECLIPSEDIR/bin/x86_64_linux
ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ECLIPSEDIR/lib/x86_64_linux

# PYTHON
RUN apt-get update && apt-get install -y -t sid python3.5 python3-pip

RUN pip3 install cython==0.22
RUN pip3 install pyclp

RUN apt-get update && apt-get install -y -t sid \
  python3-numpy python3-scipy python3-matplotlib \
  python3-pandas\
  python3-networkx
RUN pip3 install statsmodels

RUN apt-get install -y ssh
RUN ssh-keygen -t dsa -P '' -f ~/.ssh/id_dsa
RUN cat ~/.ssh/id_dsa.pub >> ~/.ssh/authorized_keys
RUN service ssh start

RUN apt-get install -y net-tools

