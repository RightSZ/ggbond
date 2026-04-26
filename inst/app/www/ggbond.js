let panelCounter = 0;
      let panels = {};
      let selectedPanelId = null;
      let selectedPanelIds = [];
      let undoStack = [];
      let sourceChoices = [];

      const SNAP_THRESHOLD = 8;
      const MAX_UNDO_STEPS = 100;

      function syncSelectionInputs() {
        Shiny.setInputValue('selected_panel_id', selectedPanelId, {priority: 'event'});
        Shiny.setInputValue('selected_panel_ids', JSON.stringify(selectedPanelIds), {priority: 'event'});
      }

      function renderSelection() {
        Object.keys(panels).forEach(function(pid) {
          let p = document.getElementById(pid);
          if (!p) return;

          if (selectedPanelIds.includes(pid)) {
            p.classList.add('selected-panel');
          } else {
            p.classList.remove('selected-panel');
          }
        });
      }

      function getPanelZIndex(el) {
        let zIndex = parseInt(el.style.zIndex, 10);
        return Number.isNaN(zIndex) ? 1 : zIndex;
      }

      function getSourceChoice(value) {
        return sourceChoices.find(function(choice) {
          return choice.value === value;
        }) || null;
      }

      function setPanelSourceMetadata(el, sourceValue) {
        let choice = getSourceChoice(sourceValue);

        if (choice && choice.type === 'image') {
          el.dataset.sourceType = 'image';
          el.dataset.aspectRatio = choice.aspect;
        } else {
          el.dataset.sourceType = 'plot';
          el.dataset.aspectRatio = '';
          el.dataset.lockAspect = 'false';
        }
      }

      function isAspectLocked(el) {
        return el.dataset.sourceType === 'image' &&
          el.dataset.lockAspect === 'true' &&
          Number.isFinite(parseFloat(el.dataset.aspectRatio)) &&
          parseFloat(el.dataset.aspectRatio) > 0;
      }

      function getAspectRatio(el) {
        return parseFloat(el.dataset.aspectRatio);
      }

      function fitSizeToAspect(width, height, aspect, maxWidth, maxHeight, preferWidth = true) {
        let fittedWidth = width;
        let fittedHeight = height;

        if (preferWidth) {
          fittedHeight = fittedWidth / aspect;
        } else {
          fittedWidth = fittedHeight * aspect;
        }

        if (fittedWidth > maxWidth) {
          fittedWidth = maxWidth;
          fittedHeight = fittedWidth / aspect;
        }

        if (fittedHeight > maxHeight) {
          fittedHeight = maxHeight;
          fittedWidth = fittedHeight * aspect;
        }

        fittedWidth = Math.max(100, Math.min(fittedWidth, maxWidth));
        fittedHeight = Math.max(80, Math.min(fittedHeight, maxHeight));

        return {
          width: fittedWidth,
          height: fittedHeight
        };
      }

      function rebuildSourceSelect(select, selectedValue = null) {
        let currentValue = selectedValue || select.value;

        select.innerHTML = '';

        sourceChoices.forEach(function(choice) {
          let option = document.createElement('option');
          option.value = choice.value;
          option.textContent = choice.label;
          select.appendChild(option);
        });

        if (currentValue && sourceChoices.some(function(choice) {
          return choice.value === currentValue;
        })) {
          select.value = currentValue;
        } else if (sourceChoices.length > 0) {
          select.value = sourceChoices[0].value;
        }
      }

      function refreshAllSourceSelects() {
        Object.keys(panels).forEach(function(id) {
          let el = document.getElementById(id);
          if (!el) return;

          let select = el.querySelector('.plot-select');
          if (!select) return;

          rebuildSourceSelect(select);
          setPanelSourceMetadata(el, select.value);
        });
      }

      function selectPanel(id, additive = false) {
        let el = document.getElementById(id);
        if (!el) return;

        if (additive) {
          if (selectedPanelIds.includes(id)) {
            selectedPanelIds = selectedPanelIds.filter(function(pid) {
              return pid !== id;
            });
          } else {
            selectedPanelIds.push(id);
          }

          selectedPanelId = selectedPanelIds.length > 0 ?
            selectedPanelIds[selectedPanelIds.length - 1] :
            null;
        } else {
          selectedPanelIds = [id];
          selectedPanelId = id;
        }

        renderSelection();
        syncSelectionInputs();
      }

      function getSelectedPanels() {
        return selectedPanelIds
          .map(function(id) {
            let el = document.getElementById(id);
            if (!el) return null;

            let left = parseFloat(el.style.left);
            let top = parseFloat(el.style.top);
            let width = parseFloat(el.style.width);
            let height = parseFloat(el.style.height);
            let zIndex = getPanelZIndex(el);

            return {
              id: id,
              el: el,
              left: left,
              top: top,
              width: width,
              height: height,
              right: left + width,
              bottom: top + height,
              centerX: left + width / 2,
              centerY: top + height / 2,
              zIndex: zIndex
            };
          })
          .filter(function(panel) {
            return panel !== null;
          });
      }

      function snapshotPanels(panelIds = null) {
        let ids = panelIds || Object.keys(panels);

        return ids
          .map(function(id) {
            let el = document.getElementById(id);
            if (!el) return null;

            return {
              id: id,
              left: parseFloat(el.style.left),
              top: parseFloat(el.style.top),
              width: parseFloat(el.style.width),
              height: parseFloat(el.style.height),
              zIndex: getPanelZIndex(el),
              lockAspect: el.dataset.lockAspect === 'true',
              showBorder: el.dataset.showBorder === 'true'
            };
          })
          .filter(function(panel) {
            return panel !== null;
          });
      }

      function snapshotsDiffer(before, after) {
        if (before.length !== after.length) return true;

        return before.some(function(panel, index) {
          let next = after[index];

          return (
            panel.id !== next.id ||
            panel.left !== next.left ||
            panel.top !== next.top ||
            panel.width !== next.width ||
            panel.height !== next.height ||
            panel.zIndex !== next.zIndex ||
            panel.lockAspect !== next.lockAspect ||
            panel.showBorder !== next.showBorder
          );
        });
      }

      function pushUndoSnapshot(snapshot) {
        if (!snapshot || snapshot.length === 0) return;

        undoStack.push(snapshot);

        if (undoStack.length > MAX_UNDO_STEPS) {
          undoStack.shift();
        }
      }

      function restoreSnapshot(snapshot) {
        if (!snapshot || snapshot.length === 0) return;

        snapshot.forEach(function(panel) {
          let el = document.getElementById(panel.id);
          if (!el) return;

          el.style.left = panel.left + 'px';
          el.style.top = panel.top + 'px';
          el.style.width = panel.width + 'px';
          el.style.height = panel.height + 'px';
          el.style.zIndex = panel.zIndex;
          el.dataset.lockAspect = panel.lockAspect ? 'true' : 'false';
          el.dataset.showBorder = panel.showBorder ? 'true' : 'false';
        });

        renderSelection();
        updateLayoutState(true);
      }

      function undoLastChange() {
        let snapshot = undoStack.pop();
        if (!snapshot) return;

        restoreSnapshot(snapshot);
      }

      function isEditingFormField(target) {
        if (!target) return false;

        return target.closest(
          'input, textarea, select, [contenteditable="true"]'
        ) !== null;
      }

      document.addEventListener('click', function(e) {
        if (!e.target || e.target.id !== 'toggle_inspector') return;

        let shell = document.getElementById('inspector_shell');
        if (!shell) return;

        let collapsed = shell.classList.toggle('is-collapsed');
        e.target.textContent = collapsed ? 'Show' : 'Hide';
        e.target.title = collapsed ? 'Show panel inspector' : 'Hide panel inspector';
      });

      function moveSelectedPanels(dx, dy) {
        let selected = getSelectedPanels();
        if (selected.length === 0) return;

        let canvas = document.getElementById('canvas');
        if (!canvas) return;

        let minDx = Math.max.apply(null, selected.map(function(panel) {
          return -panel.left;
        }));
        let maxDx = Math.min.apply(null, selected.map(function(panel) {
          return canvas.clientWidth - panel.right;
        }));
        let minDy = Math.max.apply(null, selected.map(function(panel) {
          return -panel.top;
        }));
        let maxDy = Math.min.apply(null, selected.map(function(panel) {
          return canvas.clientHeight - panel.bottom;
        }));

        let actualDx = Math.max(minDx, Math.min(dx, maxDx));
        let actualDy = Math.max(minDy, Math.min(dy, maxDy));

        if (actualDx === 0 && actualDy === 0) return;

        let before = snapshotPanels(selectedPanelIds);

        selected.forEach(function(panel) {
          panel.el.style.left = (panel.left + actualDx) + 'px';
          panel.el.style.top = (panel.top + actualDy) + 'px';
        });

        pushUndoSnapshot(before);
        updateLayoutState(true);
      }

      function alignSelectedPanels(mode) {
        let selected = getSelectedPanels();
        if (selected.length < 2) return;

        let reference = selected.find(function(panel) {
          return panel.id === selectedPanelId;
        });
        if (!reference) {
          reference = selected[selected.length - 1];
        }

        let before = snapshotPanels(selectedPanelIds);
        let target;

        if (mode === 'left') {
          target = reference.left;
          selected.forEach(function(panel) {
            panel.el.style.left = target + 'px';
          });
        }

        if (mode === 'right') {
          target = reference.right;
          selected.forEach(function(panel) {
            panel.el.style.left = (target - panel.width) + 'px';
          });
        }

        if (mode === 'center_h') {
          target = reference.centerX;
          selected.forEach(function(panel) {
            panel.el.style.left = (target - panel.width / 2) + 'px';
          });
        }

        if (mode === 'top') {
          target = reference.top;
          selected.forEach(function(panel) {
            panel.el.style.top = target + 'px';
          });
        }

        if (mode === 'bottom') {
          target = reference.bottom;
          selected.forEach(function(panel) {
            panel.el.style.top = (target - panel.height) + 'px';
          });
        }

        if (mode === 'center_v') {
          target = reference.centerY;
          selected.forEach(function(panel) {
            panel.el.style.top = (target - panel.height / 2) + 'px';
          });
        }

        if (snapshotsDiffer(before, snapshotPanels(selectedPanelIds))) {
          pushUndoSnapshot(before);
          updateLayoutState(true);
        }
      }

      function setSelectedLayer(direction) {
        let selected = getSelectedPanels();
        if (selected.length === 0) return;

        let before = snapshotPanels(Object.keys(panels));
        let selectedIds = selectedPanelIds.slice();
        let unselected = Object.keys(panels).filter(function(id) {
          return !selectedIds.includes(id);
        });
        let orderedIds = direction === 'top' ?
          unselected.concat(selectedIds) :
          selectedIds.concat(unselected);

        orderedIds.forEach(function(id, index) {
          let el = document.getElementById(id);
          if (!el) return;

          el.style.zIndex = index + 1;
        });

        if (snapshotsDiffer(before, snapshotPanels(Object.keys(panels)))) {
          pushUndoSnapshot(before);
          updateLayoutState(true);
        }
      }

      function showGuide(type, position) {
        if (type === 'v') {
          let guide = document.getElementById('guide_v');
          if (!guide) return;
          guide.style.left = position + 'px';
          guide.style.display = 'block';
        }

        if (type === 'h') {
          let guide = document.getElementById('guide_h');
          if (!guide) return;
          guide.style.top = position + 'px';
          guide.style.display = 'block';
        }
      }

      function hideGuides() {
        let gv = document.getElementById('guide_v');
        let gh = document.getElementById('guide_h');

        if (gv) gv.style.display = 'none';
        if (gh) gh.style.display = 'none';
      }

      function getSnapLines(excludeId = null) {
        let canvas = document.getElementById('canvas');

        let vLines = [
          0,
          canvas.clientWidth / 2,
          canvas.clientWidth
        ];

        let hLines = [
          0,
          canvas.clientHeight / 2,
          canvas.clientHeight
        ];

        Object.keys(panels).forEach(function(id) {
          if (id === excludeId) return;

          let el = document.getElementById(id);
          if (!el) return;

          let left = parseFloat(el.style.left);
          let top = parseFloat(el.style.top);
          let width = parseFloat(el.style.width);
          let height = parseFloat(el.style.height);

          let right = left + width;
          let bottom = top + height;
          let centerX = left + width / 2;
          let centerY = top + height / 2;

          vLines.push(left, centerX, right);
          hLines.push(top, centerY, bottom);
        });

        return {
          v: vLines,
          h: hLines
        };
      }

      function snapValue(value, candidates) {
        let best = value;
        let bestDiff = SNAP_THRESHOLD + 1;
        let guide = null;

        candidates.forEach(function(candidate) {
          let diff = Math.abs(value - candidate);

          if (diff < bestDiff && diff <= SNAP_THRESHOLD) {
            best = candidate;
            bestDiff = diff;
            guide = candidate;
          }
        });

        return {
          value: best,
          snapped: guide !== null,
          guide: guide
        };
      }

      function snapMove(left, top, width, height, excludeId) {
        let lines = getSnapLines(excludeId);

        let candidatesX = [
          { key: 'left', value: left },
          { key: 'center', value: left + width / 2 },
          { key: 'right', value: left + width }
        ];

        let candidatesY = [
          { key: 'top', value: top },
          { key: 'center', value: top + height / 2 },
          { key: 'bottom', value: top + height }
        ];

        let snappedLeft = left;
        let snappedTop = top;

        let vGuide = null;
        let hGuide = null;

        for (let i = 0; i < candidatesX.length; i++) {
          let result = snapValue(candidatesX[i].value, lines.v);

          if (result.snapped) {
            if (candidatesX[i].key === 'left') {
              snappedLeft = result.value;
            }

            if (candidatesX[i].key === 'center') {
              snappedLeft = result.value - width / 2;
            }

            if (candidatesX[i].key === 'right') {
              snappedLeft = result.value - width;
            }

            vGuide = result.guide;
            break;
          }
        }

        for (let i = 0; i < candidatesY.length; i++) {
          let result = snapValue(candidatesY[i].value, lines.h);

          if (result.snapped) {
            if (candidatesY[i].key === 'top') {
              snappedTop = result.value;
            }

            if (candidatesY[i].key === 'center') {
              snappedTop = result.value - height / 2;
            }

            if (candidatesY[i].key === 'bottom') {
              snappedTop = result.value - height;
            }

            hGuide = result.guide;
            break;
          }
        }

        hideGuides();

        if (vGuide !== null) showGuide('v', vGuide);
        if (hGuide !== null) showGuide('h', hGuide);

        return {
          left: snappedLeft,
          top: snappedTop
        };
      }

      function snapResize(left, top, width, height, excludeId) {
        let lines = getSnapLines(excludeId);

        let right = left + width;
        let bottom = top + height;

        let snappedWidth = width;
        let snappedHeight = height;

        let vGuide = null;
        let hGuide = null;

        let snapRight = snapValue(right, lines.v);
        if (snapRight.snapped) {
          snappedWidth = snapRight.value - left;
          vGuide = snapRight.guide;
        }

        let snapBottom = snapValue(bottom, lines.h);
        if (snapBottom.snapped) {
          snappedHeight = snapBottom.value - top;
          hGuide = snapBottom.guide;
        }

        hideGuides();

        if (vGuide !== null) showGuide('v', vGuide);
        if (hGuide !== null) showGuide('h', hGuide);

        return {
          width: snappedWidth,
          height: snappedHeight
        };
      }

      function updateLayoutState(force = true) {
        let state = [];

        Object.keys(panels).forEach(function(id) {
          let el = document.getElementById(id);
          if (!el) return;

          let select = el.querySelector('.plot-select');
          let header = el.querySelector('.panel-header');
          let source = select.value;
          let plot = source.startsWith('plot:') ?
            source.replace(/^plot:/, '') :
            source;

          state.push({
            id: id,
            label: el.dataset.label,
            x: parseFloat(el.style.left),
            y: parseFloat(el.style.top),
            width: parseFloat(el.style.width),
            height: parseFloat(el.style.height),
            plot: plot,
            source: source,
            lock_aspect: el.dataset.lockAspect === 'true',
            show_border: el.dataset.showBorder === 'true',
            z: getPanelZIndex(el)
          });
        });

        Shiny.setInputValue('layout_state', JSON.stringify(state), {priority: 'event'});
      }

      function addPanel(sourceId = null) {
        panelCounter += 1;

        let canvas = document.getElementById('canvas');
        let id = 'panel_' + panelCounter;
        let label = String.fromCharCode(64 + panelCounter);

        if (sourceId === null || sourceId === '') {
          sourceId = sourceChoices.length > 0 ? sourceChoices[0].value : '';
        }

        let panel = document.createElement('div');
        panel.className = 'panel-box';
        panel.id = id;
        panel.dataset.label = label;
        panel.style.left = (30 + panelCounter * 20) + 'px';
        panel.style.top = (30 + panelCounter * 20) + 'px';
        panel.style.width = '260px';
        panel.style.height = '170px';
        panel.style.zIndex = Object.keys(panels).length + 1;
        panel.dataset.lockAspect = 'false';
        panel.dataset.showBorder = 'false';

        panel.innerHTML = `
          <div class='panel-header'>Panel ${label}</div>
          <div class='panel-content'>
            <select class='plot-select'></select>
            <div style='margin-top:6px;color:#555;'>
              Click to select. Drag to move. Resize from bottom-right.
            </div>
          </div>
          <div class='resize-handle'></div>
        `;

        canvas.appendChild(panel);
        panels[id] = true;

        rebuildSourceSelect(panel.querySelector('.plot-select'), sourceId);
        setPanelSourceMetadata(panel, panel.querySelector('.plot-select').value);

        makeDraggable(panel);
        makeResizable(panel);

        panel.querySelector('.plot-select').addEventListener('change', function() {
          selectPanel(panel.id);
          setPanelSourceMetadata(panel, this.value);
          updateLayoutState(true);
        });

        selectPanel(id);
        updateLayoutState(true);
      }

      function makeDraggable(el) {
        let isDragging = false;
        let startX, startY, startLeft, startTop;
        let dragStartSnapshot = null;

        el.addEventListener('mousedown', function(e) {
          if (
            e.target.classList.contains('resize-handle') ||
            e.target.classList.contains('plot-select')
          ) {
            return;
          }

          let additive = e.ctrlKey || e.metaKey;
          if (additive && selectedPanelIds.includes(el.id)) {
            selectPanel(el.id, true);
            e.preventDefault();
            return;
          }

          selectPanel(el.id, additive);

          isDragging = true;
          dragStartSnapshot = snapshotPanels([el.id]);
          startX = e.clientX;
          startY = e.clientY;
          startLeft = parseFloat(el.style.left);
          startTop = parseFloat(el.style.top);
          e.preventDefault();
        });

        document.addEventListener('mousemove', function(e) {
          if (!isDragging) return;

          let dx = e.clientX - startX;
          let dy = e.clientY - startY;

          let canvas = document.getElementById('canvas');

          let width = parseFloat(el.style.width);
          let height = parseFloat(el.style.height);

          let maxLeft = canvas.clientWidth - width;
          let maxTop = canvas.clientHeight - height;

          let newLeft = Math.max(0, Math.min(startLeft + dx, maxLeft));
          let newTop = Math.max(0, Math.min(startTop + dy, maxTop));

          let snapped = snapMove(newLeft, newTop, width, height, el.id);

          newLeft = Math.max(0, Math.min(snapped.left, maxLeft));
          newTop = Math.max(0, Math.min(snapped.top, maxTop));

          el.style.left = newLeft + 'px';
          el.style.top = newTop + 'px';
        });

        document.addEventListener('mouseup', function() {
          if (isDragging) {
            isDragging = false;
            hideGuides();
            if (snapshotsDiffer(dragStartSnapshot, snapshotPanels([el.id]))) {
              pushUndoSnapshot(dragStartSnapshot);
            }
            dragStartSnapshot = null;
            updateLayoutState(true);
          }
        });
      }

      function makeResizable(el) {
        let handle = el.querySelector('.resize-handle');
        let isResizing = false;
        let startX, startY, startWidth, startHeight;
        let resizeStartSnapshot = null;

        handle.addEventListener('mousedown', function(e) {
          if (!selectedPanelIds.includes(el.id)) {
            selectPanel(el.id);
          }

          isResizing = true;
          resizeStartSnapshot = snapshotPanels([el.id]);
          startX = e.clientX;
          startY = e.clientY;
          startWidth = parseFloat(el.style.width);
          startHeight = parseFloat(el.style.height);
          e.preventDefault();
          e.stopPropagation();
        });

        document.addEventListener('mousemove', function(e) {
          if (!isResizing) return;

          let dx = e.clientX - startX;
          let dy = e.clientY - startY;

          let canvas = document.getElementById('canvas');

          let left = parseFloat(el.style.left);
          let top = parseFloat(el.style.top);

          let maxWidth = canvas.clientWidth - left;
          let maxHeight = canvas.clientHeight - top;

          let newWidth = Math.max(100, Math.min(startWidth + dx, maxWidth));
          let newHeight = Math.max(80, Math.min(startHeight + dy, maxHeight));
          let lockAspect = isAspectLocked(el);
          let preferWidth = Math.abs(dx) >= Math.abs(dy);

          if (lockAspect) {
            let fitted = fitSizeToAspect(
              newWidth,
              newHeight,
              getAspectRatio(el),
              maxWidth,
              maxHeight,
              preferWidth
            );
            newWidth = fitted.width;
            newHeight = fitted.height;
          }

          let snapped = snapResize(left, top, newWidth, newHeight, el.id);

          newWidth = Math.max(100, Math.min(snapped.width, maxWidth));
          newHeight = Math.max(80, Math.min(snapped.height, maxHeight));

          if (lockAspect) {
            let fitted = fitSizeToAspect(
              newWidth,
              newHeight,
              getAspectRatio(el),
              maxWidth,
              maxHeight,
              preferWidth
            );
            newWidth = fitted.width;
            newHeight = fitted.height;
          }

          el.style.width = newWidth + 'px';
          el.style.height = newHeight + 'px';
        });

        document.addEventListener('mouseup', function() {
          if (isResizing) {
            isResizing = false;
            hideGuides();
            if (snapshotsDiffer(resizeStartSnapshot, snapshotPanels([el.id]))) {
              pushUndoSnapshot(resizeStartSnapshot);
            }
            resizeStartSnapshot = null;
            updateLayoutState(true);
          }
        });
      }

      document.addEventListener('keydown', function(e) {
        if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === 'z') {
          if (isEditingFormField(e.target)) return;

          e.preventDefault();
          undoLastChange();
          return;
        }

        let arrowDeltas = {
          ArrowLeft: [-1, 0],
          ArrowRight: [1, 0],
          ArrowUp: [0, -1],
          ArrowDown: [0, 1]
        };

        if (!Object.prototype.hasOwnProperty.call(arrowDeltas, e.key)) return;
        if (isEditingFormField(e.target)) return;

        let step = e.shiftKey ? 10 : 1;
        let delta = arrowDeltas[e.key];

        e.preventDefault();
        moveSelectedPanels(delta[0] * step, delta[1] * step);
      });

      Shiny.addCustomMessageHandler('add_panel', function(message) {
        addPanel(message.source || message.plot);
      });

      Shiny.addCustomMessageHandler('update_source_choices', function(message) {
        sourceChoices = message || [];
        refreshAllSourceSelects();

        if (Object.keys(panels).length > 0) {
          updateLayoutState(true);
        }
      });

      Shiny.addCustomMessageHandler('update_panel_from_inspector', function(message) {
        let el = document.getElementById(message.id);
        if (!el) return;

        let canvas = document.getElementById('canvas');

        let x = parseFloat(message.x);
        let y = parseFloat(message.y);
        let width = parseFloat(message.width);
        let height = parseFloat(message.height);

        width = Math.max(100, Math.min(width, canvas.clientWidth));
        height = Math.max(80, Math.min(height, canvas.clientHeight));

        x = Math.max(0, Math.min(x, canvas.clientWidth - width));
        y = Math.max(0, Math.min(y, canvas.clientHeight - height));

        el.style.left = x + 'px';
        el.style.top = y + 'px';
        el.style.width = width + 'px';
        el.style.height = height + 'px';

        el.dataset.label = message.label;

        let header = el.querySelector('.panel-header');
        if (header) {
          header.innerText = 'Panel ' + message.label;
        }

        let select = el.querySelector('.plot-select');
        if (select) {
          rebuildSourceSelect(select, message.source || message.plot);
          setPanelSourceMetadata(el, select.value);
        }

        el.dataset.lockAspect = message.lock_aspect &&
          el.dataset.sourceType === 'image' ? 'true' : 'false';
        el.dataset.showBorder = message.show_border ? 'true' : 'false';

        if (isAspectLocked(el)) {
          let fitted = fitSizeToAspect(
            width,
            height,
            getAspectRatio(el),
            canvas.clientWidth - x,
            canvas.clientHeight - y,
            true
          );
          width = fitted.width;
          height = fitted.height;

          el.style.width = width + 'px';
          el.style.height = height + 'px';
        }

        selectPanel(message.id);
        updateLayoutState(true);
      });

      Shiny.addCustomMessageHandler('align_selected_panels', function(message) {
        alignSelectedPanels(message.mode);
      });

      Shiny.addCustomMessageHandler('set_selected_layer', function(message) {
        setSelectedLayer(message.direction);
      });
