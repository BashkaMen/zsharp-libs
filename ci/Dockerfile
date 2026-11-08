FROM node:24-bookworm-slim AS node

FROM ubuntu:24.04

ARG ZS_VERSION=0.7.0
ARG LLVM_VERSION=23
ARG TARGETARCH

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates curl gnupg lsb-release software-properties-common wget git pkg-config libssl-dev openssl \
 && curl -fsSL https://apt.llvm.org/llvm.sh -o /tmp/llvm.sh \
 && bash /tmp/llvm.sh ${LLVM_VERSION} \
 && apt-get install -y --no-install-recommends libclang-${LLVM_VERSION}-dev \
 && rm -rf /var/lib/apt/lists/* /tmp/llvm.sh

RUN arch=$([ "$TARGETARCH" = arm64 ] && echo aarch64 || echo x86_64) \
 && curl -fsSL "https://github.com/ZigoteProject/zsharp/releases/download/v${ZS_VERSION}/zs-${ZS_VERSION}-linux-${arch}.tar.gz" | tar -xz -C /opt \
 && ln -s /opt/zs-${ZS_VERSION}-linux-${arch} /opt/zs

COPY --from=node /usr/local/bin/node /usr/local/bin/node

ENV PATH=/opt/zs:/usr/lib/llvm-${LLVM_VERSION}/bin:$PATH
ENV ZS_LIBCLANG=/usr/lib/llvm-${LLVM_VERSION}/lib/libclang.so

RUN zs doctor
