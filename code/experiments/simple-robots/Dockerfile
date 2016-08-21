FROM ckroiss/salma-basic
RUN mkdir /simulations
RUN mkdir /simulations/simple-robots
COPY ecl-src /simulations/simple-robots/ecl-src
COPY src /simulations/simple-robots/src
COPY config_20r_100i_10ws_500x500.json /simulations/simple-robots
COPY config_3r_20i_5s_200x200.json /simulations/simple-robots
VOLUME ["/simulations/simple-robots/experiment_results"]
WORKDIR /simulations/simple-robots
ENV PYTHONPATH=/simulations/simple-robots/src:/opt/salma
ENTRYPOINT ["python3", "src/simplerobots/experiment02.py"]