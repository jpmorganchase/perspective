import perspective from "@finos/perspective";

const worker = perspective.shared_worker();

async function main() {
    if (window.location.pathname === "/" || window.location.pathname === "/index.html") {
        return;
    }

    const arrow = await fetch("../../arrow/superstore.arrow");
    const table = await worker.table(await arrow.arrayBuffer());

    const viewers = document.querySelectorAll("perspective-viewer:not(.nosuperstore)");
    for (const viewer of viewers) {
        viewer.load(table);
        viewer.toggleConfig();
    }

    let state = localStorage.getItem("lang_pref") || "Python";
    let ICON = `<span style="font-family:'Material Icons';vertical-align:bottom">input</span>`;
    let ARROW_ICON = `<span style="font-family:'Material Icons';vertical-align:bottom">arrow_forward</span>`;
    for (const pre of document.querySelectorAll("pre")) {
        const code = pre.children[0];
        if (code.classList.contains("language-html")) {
            continue;
        }
        const name = code.classList.contains("language-javascript") ? "Javascript" : "Python";
        const next = name === "Javascript" ? "Python" : "Javascript";

        pre.innerHTML =
            `<a class="toggle-language" href="#" title="Toggle to ${next}">${ICON} <span class="language">${name}</span> <span class="next">${ARROW_ICON} ${next}</span></a>` + pre.innerHTML;
        if (name !== state) {
            pre.style.display = "none";
        } else {
            pre.style.display = "block";
        }
    }

    for (const link of document.querySelectorAll("pre a")) {
        link.addEventListener("click", event => {
            event.preventDefault();
            state = state === "Python" ? "Javascript" : "Python";
            localStorage.setItem("lang_pref", state);
            for (const pre of document.querySelectorAll("pre")) {
                const code = pre.children[1];
                if (!code || code.classList.contains("language-html")) {
                    continue;
                }
                const name = code.classList.contains("language-javascript") ? "Javascript" : "Python";
                if (name !== state) {
                    pre.style.display = "none";
                } else if (name !== "html") {
                    pre.style.display = "block";
                }
            }
        });
    }
}

window.addEventListener("DOMContentLoaded", () => {
    main();
});
