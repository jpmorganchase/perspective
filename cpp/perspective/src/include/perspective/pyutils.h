/******************************************************************************
 *
 * Copyright (c) 2017, the Perspective Authors.
 *
 * This file is part of the Perspective library, distributed under the terms of
 * the Apache License 2.0.  The full license can be found in the LICENSE file.
 *
 */

#pragma once
#include <perspective/first.h>

#ifdef PSP_ENABLE_PYTHON
#include <thread>
#include <boost/thread/locks.hpp>
#include <boost/thread/shared_mutex.hpp>

namespace perspective {

class PERSPECTIVE_EXPORT PerspectiveScopedGILRelease {
    public:
        PerspectiveScopedGILRelease(std::thread::id event_loop_thread_id, boost::shared_mutex* lock, bool is_read);
        ~PerspectiveScopedGILRelease();
    private:
        PyThreadState* m_thread_state;
        bool m_is_read;
        intptr_t m_lock;
};


void _set_event_loop();

} // namespace perspective
#endif