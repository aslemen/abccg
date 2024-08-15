FROM ubuntu:latest

ENV LANG C.UTF-8
ENV TZ Asia/Tokyo

# ============
# System packages
# ============
# Collect packages
RUN apt update \
    && DEBIAN_FRONTEND=noninteractive apt install -y --no-install-recommends sudo wget curl ssh git zip locales ed less neovim ca-certificates jq python3 python3-pip python3-venv build-essential 

# ============
# User settings
# ============
ARG USER_NAME=vscode
ARG UID=1000
ARG GROUP_NAME=vscode
ARG GID=1000
ARG GID_WHEEL=999
ARG USER_SHELL=/bin/bash
ARG HOME="/home/${USER_NAME}"
ARG LOCAL_BIN="${HOME}/.local/bin"

ENV XDG_CONFIG_HOME="${HOME}/.config"
ENV XDG_CACHE_HOME="${HOME}/.cache"
ENV XDG_DATA_HOME="${HOME}/.local/share"
ENV XDG_STATE_HOME="${HOME}/.local/state"
ENV PATH="${LOCAL_BIN}:${PATH}"

RUN : \
    && groupadd --gid ${GID_WHEEL} wheel \
    && echo %wheel ALL=\(ALL\) NOPASSWD:ALL >> /etc/sudoers \
    && if getent group ${GID} >/dev/null 2>&1; then \
        groupmod -n ${GROUP_NAME} $(getent group ${GID} | cut -d: -f1) \
        ; \
    else \
        groupadd --gid ${GID} ${GROUP_NAME} \
        ; \
    fi \
    && if id -u ${UID} >/dev/null 2>&1; then \
        usermod \
            -l ${USER_NAME} \
            -d ${HOME} \
            -g ${GROUP_NAME} \
            -aG wheel \
            $(id -nu ${UID}) \
        && chsh -s ${USER_SHELL} ${USER_NAME} \
        && mkdir ${HOME} \
        && chown -R ${USER_NAME}:${GROUP_NAME} ${HOME} \
        ; \
    else \
        useradd --create-home \
                --uid ${UID} \
                --shell ${USER_SHELL} \
                -g ${GROUP_NAME} \
                -G wheel \
                ${USER_NAME}\
        ; \
    fi

# ============
# Packages
# ============

# ------------
# Initialization
# ------------
# do not switch shell; keep using bash
# switch user
USER ${USER_NAME}

# ensure directories
RUN mkdir -p \
    ${XDG_CONFIG_HOME} \
    ${XDG_CACHE_HOME} \
    ${XDG_DATA_HOME} \
    ${XDG_STATE_HOME} \
    ${LOCAL_BIN} \
    ${HOME}

# ------------
# Shell settings
# ------------
# RUN echo "\
# export \\\
#     XDG_CONFIG_HOME=\${HOME}/.config \\\
#     XDG_CACHE_HOME=\${HOME}/.cache \\\
#     XDG_DATA_HOME=\${HOME}/.local/share \\\
#     XDG_STATE_HOME=\${HOME}/.local/state \\\
#     ZDOTDIR=\${XDG_CONFIG_HOME}/zsh" >> /etc/zsh/zshenv



# ------------
# Rust
# ------------
ENV RUSTUP_HOME=${XDG_DATA_HOME}/rustup
ENV CARGO_HOME=${XDG_DATA_HOME}/cargo
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
RUN echo 'source ${HOME}/.local/share/cargo/env' >> ${HOME}/.bashrc




# ============
# Entrypoint
# ============
CMD ["/bin/bash"]
