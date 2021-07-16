const siteConfig = {
    title: "Perspective",
    tagline: "Streaming Analytics via WebAssembly",
    url: "https://perspective.finos.org/",
    cname: "perspective.finos.org",
    baseUrl: "/",
    projectName: "perspective",
    organizationName: "finos",
    headerLinks: [
        {doc: "md/js", label: "Docs"},
        {href: "https://github.com/finos/perspective/", label: "GitHub"}
    ],
    headerIcon: "img/logo.png",
    favicon: "img/favicon.png",
    colors: {
        primaryColor: "#242526",
        secondaryColor: "#242526"
    },
    users: [
        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/803de90736a3641ad91c5c7a1b49d0a7/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/803de90736a3641ad91c5c7a1b49d0a7",
            caption: "Superstore"
        },
        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/efd4a857aca9a52ab6cddbb6e1f701c9/c6c0fb7611ca742830e05cce667678c25b6f288a/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/efd4a857aca9a52ab6cddbb6e1f701c9",
            caption: "Olympics"
        },
        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/c42f3189699bd29cf20bbe7dce767b07/62d75a47e049602312ba2597bfd37eb032b156f0/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/c42f3189699bd29cf20bbe7dce767b07",
            caption: "Styled"
        },

        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/45b868833c9f456bd39a51e606412c5d/e590d237a5237790694946018680719c9fef56cb/thumbnail.png ",
            infoLink: "https://bl.ocks.org/texodus/45b868833c9f456bd39a51e606412c5d",
            caption: "Editable"
        },
        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/9bec2f8041471bafc2c56db2272a9381/c69c2cfacb23015f3aaeab3555a0035702ffdb1c/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/9bec2f8041471bafc2c56db2272a9381",
            caption: "Streaming"
        },
        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/02d8fd10aef21b19d6165cf92e43e668/5e78be024893aa651fcdfac816841d54777ccdec/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/02d8fd10aef21b19d6165cf92e43e668",
            caption: "CSV"
        },

        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/eb151fdd9f98bde987538cbc20e003f6/79d409006f50b24f1607758945144b392e4921a2/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/eb151fdd9f98bde987538cbc20e003f6",
            caption: "IEX Cloud"
        },
        {
            pinned: true,
            image: "https://bl.ocks.org/texodus/raw/bc8d7e6f72e09c9dbd7424b4332cacad/f704ce53a3f453f8fe66bd9ff4ead831786384ea/thumbnail.png",
            infoLink: "https://bl.ocks.org/texodus/bc8d7e6f72e09c9dbd7424b4332cacad",
            caption: "NYC Citibike"
        },
        {
            pinned: true,
            image: "https://perspective.finos.org/img/jupyterlab.png",
            infoLink: "http://beta.mybinder.org/v2/gh/finos/perspective/master?urlpath=lab/tree/examples/jupyter-notebooks",
            caption: "JupyterLab"
        }
    ],
    copyright: "Copyright © " + new Date().getFullYear() + " Perspective Authors",
    highlight: {
        theme: "atom-one-light"
    },
    scripts: ["https://buttons.github.io/buttons.js", "js/index.js"],
    stylesheets: [
        {rel: "preconnect", href: "https://fonts.gstatic.com", crossOrigin: "anonymous"},
        {rel: "preload", href: "https://fonts.gstatic.com/s/materialicons/v92/flUhRq6tzZclQEJ-Vdg-IuiaDsNcIhQ8tQ.woff2", as: "font", crossOrigin: "anonymous"},
        {rel: "preload", href: "https://fonts.gstatic.com/s/opensans/v20/mem8YaGs126MiZpBA-UFVZ0bf8pkAg.woff2", as: "font", crossOrigin: "anonymous"},
        {rel: "preload", href: "https://fonts.gstatic.com/s/opensans/v20/mem5YaGs126MiZpBA-UN_r8OUuhpKKSTjw.woff2", as: "font", crossOrigin: "anonymous"},
        {rel: "preload", href: "https://fonts.gstatic.com/s/robotomono/v13/L0xTDF4xlVMF-BfR8bXMIhJHg45mwgGEFl0_3vvtSM1J-gEPT5Ese09hHSh0me8iUI0.woff2", as: "font", crossOrigin: "anonymous"},
        {rel: "preload", href: "https://fonts.gstatic.com/s/orbitron/v17/yMJMMIlzdpvBhQQL_SC3X9yhF25-T1nysimBoWg1fDAlp7lk.woff", as: "font", crossOrigin: "anonymous"},
        {rel: "preload", href: "https://fonts.gstatic.com/s/publicsans/v5/ijwGs572Xtc6ZYQws9YVwllKVG8qX1oyOymuFpmJxAcufVotfzbj.woff", as: "font", crossOrigin: "anonymous"},
        "https://fonts.googleapis.com/css?family=Material+Icons",
        "https://fonts.googleapis.com/css?family=Open+Sans",
        "https://fonts.googleapis.com/css?family=Public+Sans",
        "https://fonts.googleapis.com/css?family=Roboto+Mono",
        "https://fonts.googleapis.com/css2?family=Orbitron:wght@900"
    ],
    onPageNav: "separate",
    ogImage: "img/perspective.png",
    twitterImage: "img/perspective.png"
};

module.exports = siteConfig;
