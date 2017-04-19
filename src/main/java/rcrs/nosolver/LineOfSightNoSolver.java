package rcrs.nosolver;

import rescuecore2.config.Config;
import rescuecore2.misc.Pair;
import rescuecore2.misc.geometry.GeometryTools2D;
import rescuecore2.misc.geometry.Line2D;
import rescuecore2.misc.geometry.Point2D;
import rescuecore2.misc.geometry.Vector2D;
import rescuecore2.standard.entities.*;
import rescuecore2.standard.entities.Edge;
import rescuecore2.worldmodel.Entity;
import rescuecore2.worldmodel.WorldModel;

import java.util.*;

/**
   Line of sight calculation.

   Adapted from rescuecore2.standard.kernel.LineOfSightPerception
 */
public class LineOfSightNoSolver {
    private static final int DEFAULT_VIEW_DISTANCE = 30000;
    private static final int DEFAULT_HP_PRECISION = 1000;
    private static final int DEFAULT_DAMAGE_PRECISION = 100;
    private static final int DEFAULT_RAY_COUNT = 720;

    private static final String VIEW_DISTANCE_KEY = "perception.los.max-distance";
    private static final String RAY_COUNT_KEY = "perception.los.ray-count";
    private static final String HP_PRECISION_KEY = "perception.los.precision.hp";
    private static final String DAMAGE_PRECISION_KEY = "perception.los.precision.damage";

    private static final IntersectionSorter INTERSECTION_SORTER = new IntersectionSorter();

    private int viewDistance;
    private int rayCount;

    private StandardWorldModel world;

    public LineOfSightNoSolver(Config config, WorldModel<? extends Entity> model) {
        world = StandardWorldModel.createStandardWorldModel(model);
        viewDistance = config.getIntValue(VIEW_DISTANCE_KEY, DEFAULT_VIEW_DISTANCE);
        rayCount = config.getIntValue(RAY_COUNT_KEY, DEFAULT_RAY_COUNT);
    }

    public Collection<StandardEntity> getVisibleEntities(Position agentPosition) {
        Point2D point = new Point2D(agentPosition.x(), agentPosition.y());
        Collection<StandardEntity> nearby = world.getObjectsInRange((int)agentPosition.x(), (int)agentPosition.y(), viewDistance);

        return findVisible(point, nearby);
    }

    private Collection<StandardEntity> findVisible(Point2D location, Collection<StandardEntity> nearby) {
        Collection<LineInfo> lines = getAllLines(nearby);
        double dAngle = Math.PI * 2 / rayCount;
        Collection<StandardEntity> result = new HashSet<StandardEntity>();
        for (int i = 0; i < rayCount; ++i) {
            double angle = i * dAngle;
            Vector2D vector = new Vector2D(Math.sin(angle), Math.cos(angle)).scale(viewDistance);
            Ray ray = new Ray(new Line2D(location, vector), lines);
            for (LineInfo hit : ray.getLinesHit()) {
                StandardEntity e = hit.getEntity();
                result.add(e);
            }
        }
        return result;
    }

    private Collection<LineInfo> getAllLines(Collection<StandardEntity> entities) {
        Collection<LineInfo> result = new HashSet<LineInfo>();
        for (StandardEntity next : entities) {
            if (next instanceof Building) {
                for (Edge edge : ((Building)next).getEdges()) {
                    Line2D line = edge.getLine();
                    result.add(new LineInfo(line, next, !edge.isPassable()));
                }
            }
            if (next instanceof Road) {
                for (Edge edge : ((Road)next).getEdges()) {
                    Line2D line = edge.getLine();
                    result.add(new LineInfo(line, next, false));
                }
            }
            else if (next instanceof Blockade) {
                int[] apexes = ((Blockade)next).getApexes();
                List<Point2D> points = GeometryTools2D.vertexArrayToPoints(apexes);
                List<Line2D> lines = GeometryTools2D.pointsToLines(points, true);
                for (Line2D line : lines) {
                    result.add(new LineInfo(line, next, false));
                }
            }
            else {
                continue;
            }
        }
        return result;
    }

    private static class Ray {
        /** The ray itself. */
        private Line2D ray;
        /** The visible length of the ray. */
        private double length;
        /** List of lines hit in order. */
        private List<LineInfo> hit;

        public Ray(Line2D ray, Collection<LineInfo> otherLines) {
            this.ray = ray;
            List<Pair<LineInfo, Double>> intersections = new ArrayList<Pair<LineInfo, Double>>();
            // Find intersections with other lines
            for (LineInfo other : otherLines) {
                double d1 = ray.getIntersection(other.getLine());
                double d2 = other.getLine().getIntersection(ray);
                if (d2 >= 0 && d2 <= 1 && d1 > 0 && d1 <= 1) {
                    intersections.add(new Pair<LineInfo, Double>(other, d1));
                }
            }
            Collections.sort(intersections, INTERSECTION_SORTER);
            hit = new ArrayList<LineInfo>();
            length = 1;
            for (Pair<LineInfo, Double> next : intersections) {
                LineInfo l = next.first();
                hit.add(l);
                if (l.isBlocking()) {
                    length = next.second();
                    break;
                }
            }
        }

        public Line2D getRay() {
            return ray;
        }

        public double getVisibleLength() {
            return length;
        }

        public List<LineInfo> getLinesHit() {
            return Collections.unmodifiableList(hit);
        }
    }

    private static class LineInfo {
        private Line2D line;
        private StandardEntity entity;
        private boolean blocking;

        public LineInfo(Line2D line, StandardEntity entity, boolean blocking) {
            this.line = line;
            this.entity = entity;
            this.blocking = blocking;
        }

        public Line2D getLine() {
            return line;
        }

        public StandardEntity getEntity() {
            return entity;
        }

        public boolean isBlocking() {
            return blocking;
        }
    }

    private static class IntersectionSorter implements Comparator<Pair<LineInfo, Double>>, java.io.Serializable {
        @Override
        public int compare(Pair<LineInfo, Double> a, Pair<LineInfo, Double> b) {
            double d1 = a.second();
            double d2 = b.second();
            if (d1 < d2) {
                return -1;
            }
            if (d1 > d2) {
                return 1;
            }
            return 0;
        }
    }

}
