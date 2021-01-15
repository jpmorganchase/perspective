/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

#include <perspective/first.h>
#include <perspective/base.h>
#include <perspective/pool.h>
#include <perspective/update_task.h>
#include <perspective/compat.h>
#include <perspective/env_vars.h>
#ifdef PSP_ENABLE_PYTHON
#include <thread>
#endif
#include <chrono>

namespace perspective {

t_updctx::t_updctx() {}

t_updctx::t_updctx(t_uindex gnode_id, const std::string& ctx)
    : m_gnode_id(gnode_id)
    , m_ctx(ctx) {}

#if defined PSP_ENABLE_WASM

t_val
empty_callback() {
    t_val callback = t_val::global("Object").new_();
    callback.set("_update_callback", t_val::global("Function").new_());
    return callback;
}

t_pool::t_pool()
    : m_update_delegate(empty_callback()) 
    , m_sleep(0) {
        m_run.clear();
    }

#elif defined PSP_ENABLE_PYTHON

t_val
empty_callback() {
    return py::none();
}

t_pool::t_pool()
    : m_update_delegate(empty_callback())
    , m_event_loop_thread_id(std::thread::id())
    , m_lock(new boost::shared_mutex())
    , m_sleep(0) {
        m_run.clear();
    }

#else

t_pool::t_pool()
    : m_sleep(0) {
        m_run.clear();
    }

#endif

t_pool::~t_pool() {
#ifdef PSP_ENABLE_PYTHON
    delete m_lock;
#endif
}

void
t_pool::init() {
    if (t_env::log_progress()) {
        std::cout << "t_pool.init " << std::endl;
    }
    m_run.test_and_set(std::memory_order_acquire);
    m_data_remaining.store(false);
    std::thread t(&t_pool::_process, this);
    set_thread_name(t, "psp_pool_thread");
    t.detach();
}

t_uindex
t_pool::register_gnode(t_gnode* node) {
    std::lock_guard<std::mutex> lg(m_mtx);

    m_gnodes.push_back(node);
    t_uindex id = m_gnodes.size() - 1;
    node->set_id(id);
    node->set_pool_cleanup([this, id]() { this->m_gnodes[id] = 0; });
#ifdef PSP_ENABLE_PYTHON
    if (m_event_loop_thread_id != std::thread::id()) {
        node->set_lock(m_lock);
        node->set_event_loop_thread_id(m_event_loop_thread_id);
    }
#endif

    if (t_env::log_progress()) {
        std::cout << "t_pool.register_gnode node => " << node << " rv => " << id << std::endl;
    }

    return id;
}

void
t_pool::unregister_gnode(t_uindex idx) {
    std::lock_guard<std::mutex> lgxo(m_mtx);

    if (t_env::log_progress()) {
        std::cout << "t_pool.unregister_gnode idx => " << idx << std::endl;
    }

    m_gnodes[idx] = 0;
}

void
t_pool::send(t_uindex gnode_id, t_uindex port_id, const t_data_table& table) {
    {
        std::lock_guard<std::mutex> lg(m_mtx);
        m_data_remaining.store(true);

        if (m_gnodes[gnode_id]) {
            m_gnodes[gnode_id]->send(port_id, table);
        }

        if (t_env::log_progress()) {
            std::cout << "t_pool.send gnode_id => " << gnode_id << " port_id => " << port_id
                      << " tbl_size => " << table.size() << std::endl;
        }

        if (t_env::log_data_pool_send()) {
            std::cout << "t_pool.send" << std::endl;
            table.pprint();
        }
    }
}

#ifdef PSP_ENABLE_PYTHON
void t_pool::set_event_loop() {
    m_event_loop_thread_id = std::this_thread::get_id();
    for (auto node : m_gnodes) {
        node->set_event_loop_thread_id(m_event_loop_thread_id);
        node->set_lock(m_lock);
    }
}

boost::shared_mutex* t_pool::get_lock() const {
    return m_lock;
}

std::thread::id t_pool::get_event_loop_thread_id() const {
    return m_event_loop_thread_id;
}
#endif

void
t_pool::_process() {
    auto work_to_do = m_data_remaining.load();
    if (work_to_do) {
        t_update_task task(*this);
        task.run();
    }
}

void
t_pool::stop() {
    m_run.clear(std::memory_order_release);
    _process();
    if (t_env::log_progress()) {
        std::cout << "t_pool.stop" << std::endl;
    }
}

void
t_pool::set_sleep(t_uindex ms) {
    m_sleep.store(ms);
    if (t_env::log_progress()) {
        std::cout << "t_pool.set_sleep ms => " << ms << std::endl;
    }
}

std::vector<t_stree*>
t_pool::get_trees() {
    std::vector<t_stree*> rval;
    for (auto& g : m_gnodes) {
        if (!g)
            continue;
        auto trees = g->get_trees();
        rval.insert(std::end(rval), std::begin(trees), std::end(trees));
    }

    if (t_env::log_progress()) {
        std::cout << "t_pool.get_trees: "
                  << " rv => " << rval << std::endl;
    }

    return rval;
}

#ifdef PSP_ENABLE_WASM
void
t_pool::register_context(
    t_uindex gnode_id, const std::string& name, t_ctx_type type, std::int32_t ptr) {
    std::lock_guard<std::mutex> lg(m_mtx);
    if (!validate_gnode_id(gnode_id))
        return;
    m_gnodes[gnode_id]->_register_context(name, type, ptr);
}

#else
void
t_pool::register_context(
    t_uindex gnode_id, const std::string& name, t_ctx_type type, std::int64_t ptr) {
    std::lock_guard<std::mutex> lg(m_mtx);
    if (!validate_gnode_id(gnode_id))
        return;
    m_gnodes[gnode_id]->_register_context(name, type, ptr);
}
#endif

#if defined PSP_ENABLE_WASM || defined PSP_ENABLE_PYTHON
void
t_pool::set_update_delegate(t_val ud) {
    m_update_delegate = ud;
}
#endif

void
t_pool::notify_userspace(t_uindex port_id) {
    #if defined PSP_ENABLE_WASM
        m_update_delegate.call<void>("_update_callback", port_id);
    #elif PSP_ENABLE_PYTHON
        if (!m_update_delegate.is_none()) {
            m_update_delegate.attr("_update_callback")(port_id);
        }
    #endif
}

void
t_pool::unregister_context(t_uindex gnode_id, const std::string& name) {
    std::lock_guard<std::mutex> lg(m_mtx);

    if (t_env::log_progress()) {
        std::cout << repr() << " << t_pool.unregister_context: "
                  << " gnode_id => " << gnode_id << " name => " << name << std::endl;
    }

    if (!validate_gnode_id(gnode_id))
        return;
    m_gnodes[gnode_id]->_unregister_context(name);
}

bool
t_pool::get_data_remaining() const {
    auto data = m_data_remaining.load();
    return data;
}

std::vector<t_tscalar>
t_pool::get_row_data_pkeys(t_uindex gnode_id, const std::vector<t_tscalar>& pkeys) {
    std::lock_guard<std::mutex> lg(m_mtx);

    if (!validate_gnode_id(gnode_id))
        return std::vector<t_tscalar>();

    auto rv = m_gnodes[gnode_id]->get_row_data_pkeys(pkeys);

    if (t_env::log_progress()) {
        std::cout << "t_pool.get_row_data_pkeys: "
                  << " gnode_id => " << gnode_id << " pkeys => " << pkeys << " rv => " << rv
                  << std::endl;
    }

    return rv;
}

std::vector<t_updctx>
t_pool::get_contexts_last_updated() {
    std::lock_guard<std::mutex> lg(m_mtx);
    std::vector<t_updctx> rval;

    for (t_uindex idx = 0, loop_end = m_gnodes.size(); idx < loop_end; ++idx) {
        if (!m_gnodes[idx])
            continue;

        auto updated_contexts = m_gnodes[idx]->get_contexts_last_updated();
        auto gnode_id = m_gnodes[idx]->get_id();

        for (const auto& ctx_name : updated_contexts) {
            if (t_env::log_progress()) {
                std::cout << "t_pool.get_contexts_last_updated: "
                          << " gnode_id => " << gnode_id << " ctx_name => " << ctx_name
                          << std::endl;
            }
            rval.push_back(t_updctx(gnode_id, ctx_name));
        }
    }
    return rval;
}

bool
t_pool::validate_gnode_id(t_uindex gnode_id) const {
    return m_gnodes[gnode_id] && gnode_id < m_gnodes.size();
}

std::string
t_pool::repr() const {
    std::stringstream ss;
    ss << "t_pool<" << this << ">";
    return ss.str();
}

void
t_pool::pprint_registered() const {
    auto self = repr();

    for (t_uindex idx = 0, loop_end = m_gnodes.size(); idx < loop_end; ++idx) {
        if (!m_gnodes[idx])
            continue;
        auto gnode_id = m_gnodes[idx]->get_id();
        auto ctxnames = m_gnodes[idx]->get_registered_contexts();

        for (const auto& cname : ctxnames) {
            std::cout << self << " gnode_id => " << gnode_id << " ctxname => " << cname
                      << std::endl;
        }
    }
}

t_uindex
t_pool::epoch() const {
    return m_epoch.load();
}

void
t_pool::inc_epoch() {
    ++m_epoch;
}

std::vector<t_uindex>
t_pool::get_gnodes_last_updated() {
    std::lock_guard<std::mutex> lg(m_mtx);
    std::vector<t_uindex> rv;

    for (t_uindex idx = 0, loop_end = m_gnodes.size(); idx < loop_end; ++idx) {
        if (!m_gnodes[idx] || !m_gnodes[idx]->was_updated())
            continue;

        rv.push_back(idx);
        m_gnodes[idx]->clear_updated();
    }
    return rv;
}

t_gnode*
t_pool::get_gnode(t_uindex idx) {
    std::lock_guard<std::mutex> lg(m_mtx);
    PSP_VERBOSE_ASSERT(idx < m_gnodes.size() && m_gnodes[idx], "Bad gnode encountered");
    return m_gnodes[idx];
}

} // end namespace perspective
