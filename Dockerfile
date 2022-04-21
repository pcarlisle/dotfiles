# syntax=docker/dockerfile:1.2
FROM debian:bullseye

ARG user=patrick
ARG TERM=xterm-256color

ENV TERM=${TERM}

RUN apt-get update -qq && apt-get install -qq \
    zsh \
    curl \
    sudo \
    git \
    file \
    vim

RUN useradd --shell /bin/zsh --user-group --create-home "${user}"

RUN echo "${user} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

WORKDIR /home/${user}

COPY --chown=$user:$user . /home/$user/dotfiles

USER $user

RUN /home/$user/dotfiles/install.sh

CMD ["/bin/zsh", "-l"]
